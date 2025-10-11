import * as pulumi from "@pulumi/pulumi";
import * as aws from "@pulumi/aws";

const rateLimit = 100;
const burstLimit = 200;
const quotaLimit = 20000;

const apiGatewayCloudWatchRole = new aws.iam.Role("api-gateway-cloudwatch-role", {
  assumeRolePolicy: JSON.stringify({
    Version: "2012-10-17",
    Statement: [{
      Action: "sts:AssumeRole",
      Principal: {
        Service: "apigateway.amazonaws.com"
      },
      Effect: "Allow",
      Sid: ""
    }]
  })
});

new aws.iam.RolePolicyAttachment("api-gateway-cloudwatch-policy", {
  role: apiGatewayCloudWatchRole.name,
  policyArn: "arn:aws:iam::aws:policy/service-role/AmazonAPIGatewayPushToCloudWatchLogs"
});

const account = new aws.apigateway.Account("api-gateway-account", {
  cloudwatchRoleArn: apiGatewayCloudWatchRole.arn
});

const wafWebAcl = new aws.wafv2.WebAcl("rate-limiter-waf", {
  scope: "CLOUDFRONT",
  description: "WAF for IP-based rate limiting",
  defaultAction: {
    allow: {}
  },
  rules: [
    {
      name: "IPRateLimit",
      priority: 1,
      statement: {
        rateBasedStatement: {
          limit: 1000,
          aggregateKeyType: "IP"
        }
      },
      action: {
        block: {}
      },
      visibilityConfig: {
        sampledRequestsEnabled: true,
        cloudwatchMetricsEnabled: true,
        metricName: "IPRateLimit"
      }
    }
  ],
  visibilityConfig: {
    sampledRequestsEnabled: true,
    cloudwatchMetricsEnabled: true,
    metricName: "RateLimiterWAF"
  }
}, {
  provider: new aws.Provider("us-east-1-provider", { region: "us-east-1" })
});

const api = new aws.apigateway.RestApi("rate-limiter-api", {
  description: "Rate Limiter API with Token Bucket algorithm",
  endpointConfiguration: {
    types: "REGIONAL"
  }
});

const resource = new aws.apigateway.Resource("api-resource", {
  restApi: api.id,
  parentId: api.rootResourceId,
  pathPart: "check"
});

const method = new aws.apigateway.Method("api-method", {
  restApi: api.id,
  resourceId: resource.id,
  httpMethod: "GET",
  authorization: "NONE",
  apiKeyRequired: true
});

const integration = new aws.apigateway.Integration("api-integration", {
  restApi: api.id,
  resourceId: resource.id,
  httpMethod: method.httpMethod,
  type: "MOCK",
  requestTemplates: {
    "application/json": '{"statusCode": 200}'
  },
  passthroughBehavior: "WHEN_NO_MATCH"
});

const methodResponse = new aws.apigateway.MethodResponse("method-response", {
  restApi: api.id,
  resourceId: resource.id,
  httpMethod: method.httpMethod,
  statusCode: "200",
  responseModels: {
    "application/json": "Empty"
  },
  responseParameters: {
    "method.response.header.X-RateLimit-Limit": true,
    "method.response.header.X-RateLimit-Burst": true,
    "method.response.header.X-RateLimit-Quota": true
  }
});

const integrationResponse = new aws.apigateway.IntegrationResponse("integration-response", {
  restApi: api.id,
  resourceId: resource.id,
  httpMethod: method.httpMethod,
  statusCode: methodResponse.statusCode,
  responseParameters: {
    "method.response.header.X-RateLimit-Limit": `'${rateLimit}'`,
    "method.response.header.X-RateLimit-Burst": `'${burstLimit}'`,
    "method.response.header.X-RateLimit-Quota": `'${quotaLimit}'`
  },
  responseTemplates: {
    "application/json": '{"message": "Rate limit check passed"}'
  }
}, { dependsOn: [integration, methodResponse] });

const throttleResponse = new aws.apigateway.Response("throttle-response", {
  restApiId: api.id,
  responseType: "THROTTLED",
  statusCode: "429",
  responseParameters: {
    "gatewayresponse.header.X-RateLimit-Limit": `'${rateLimit}'`,
    "gatewayresponse.header.X-RateLimit-Burst": `'${burstLimit}'`
  },
  responseTemplates: {
    "application/json": '{"message": "Rate limit exceeded"}'
  }
});

const quotaExceededResponse = new aws.apigateway.Response("quota-exceeded-response", {
  restApiId: api.id,
  responseType: "QUOTA_EXCEEDED",
  statusCode: "429",
  responseParameters: {
    "gatewayresponse.header.X-RateLimit-Quota": `'${quotaLimit}'`
  },
  responseTemplates: {
    "application/json": '{"message": "Daily quota exceeded"}'
  }
});

const deployment = new aws.apigateway.Deployment("api-deployment", {
  restApi: api.id,
  triggers: {
    redeployment: JSON.stringify([method, integration, integrationResponse, throttleResponse, quotaExceededResponse])
  }
}, {
  dependsOn: [method, integration, integrationResponse, throttleResponse, quotaExceededResponse]
});

const stage = new aws.apigateway.Stage("api-stage", {
  restApi: api.id,
  deployment: deployment.id,
  stageName: "prod",
  description: "Production stage with rate limiting",
  variables: {
    rateLimit: rateLimit.toString(),
    burstLimit: burstLimit.toString()
  }
});

const methodSettings = new aws.apigateway.MethodSettings("method-settings", {
  restApi: api.id,
  stageName: stage.stageName,
  methodPath: "*/*",
  settings: {
    throttlingRateLimit: rateLimit,
    throttlingBurstLimit: burstLimit,
    metricsEnabled: true,
    loggingLevel: "INFO",
    dataTraceEnabled: true
  }
}, { dependsOn: [stage, account] });

const usagePlan = new aws.apigateway.UsagePlan("rate-limiter-usage-plan", {
  description: "Token Bucket based rate limiting",
  throttleSettings: {
    rateLimit,
    burstLimit
  },
  quotaSettings: {
    limit: quotaLimit,
    period: "DAY"
  },
  apiStages: [{
    apiId: api.id,
    stage: stage.stageName
  }]
}, { dependsOn: [stage, methodSettings] });

//! 실제로는 AWS SDK를 활용한 API Key 생성 로직이 필요할 것
const apiKey = new aws.apigateway.ApiKey("rate-limiter-api-key", {
  description: "API key for rate limiting"
});

new aws.apigateway.UsagePlanKey("usage-plan-key", {
  keyId: apiKey.id,
  keyType: "API_KEY",
  usagePlanId: usagePlan.id
});

const distribution = new aws.cloudfront.Distribution("rate-limiter-cdn", {
  enabled: true,
  comment: "Rate Limiter CDN with WAF and geo-restriction",
  origins: [{
    domainName: pulumi.interpolate`${api.id}.execute-api.${aws.config.region}.amazonaws.com`,
    originId: "api-gateway",
    originPath: pulumi.interpolate`/${stage.stageName}`,
    customOriginConfig: {
      httpPort: 80,
      httpsPort: 443,
      originProtocolPolicy: "https-only",
      originSslProtocols: ["TLSv1.2"]
    }
  }],
  defaultCacheBehavior: {
    targetOriginId: "api-gateway",
    viewerProtocolPolicy: "redirect-to-https",
    allowedMethods: ["DELETE", "GET", "HEAD", "OPTIONS", "PATCH", "POST", "PUT"],
    cachedMethods: ["GET", "HEAD"],
    forwardedValues: {
      queryString: true,
      headers: ["x-api-key"],
      cookies: {
        forward: "none"
      }
    },
    minTtl: 0,
    defaultTtl: 0,
    maxTtl: 0
  },
  restrictions: {
    geoRestriction: {
      restrictionType: "whitelist",
      locations: ["KR"]
    }
  },
  viewerCertificate: {
    cloudfrontDefaultCertificate: true
  },
  webAclId: wafWebAcl.arn
});

new aws.cloudwatch.MetricAlarm("rate-limit-alarm", {
  comparisonOperator: "GreaterThanThreshold",
  evaluationPeriods: 1,
  metricName: "IPRateLimit",
  namespace: "AWS/WAFV2",
  period: 300,
  statistic: "Sum",
  threshold: 100,
  alarmDescription: "Alert when IP rate limit is exceeded",
  dimensions: {
    Rule: "IPRateLimit",
    WebACL: wafWebAcl.name,
    Region: "us-east-1"
  }
});

export const cloudfrontUrl = distribution.domainName;
export const apiUrl = stage.invokeUrl;
export const apiKeyId = apiKey.id;
export const apiKeyValue = apiKey.value;
