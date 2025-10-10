import * as pulumi from "@pulumi/pulumi";
import * as aws from "@pulumi/aws";
import * as awsx from "@pulumi/awsx";
import * as command from "@pulumi/command";
import { SubnetAllocationStrategy } from '@pulumi/awsx/ec2';

const vpc = new awsx.ec2.Vpc("rate-limiter-vpc", {
  cidrBlock: "10.0.0.0/16",
  numberOfAvailabilityZones: 2,
  subnetSpecs: [
    { type: awsx.ec2.SubnetType.Private, cidrMask: 19 },
    { type: awsx.ec2.SubnetType.Public, cidrMask: 20 }
  ],
  subnetStrategy: SubnetAllocationStrategy.Auto,
  natGateways: {
    strategy: awsx.ec2.NatGatewayStrategy.Single
  }
});

const redisSecurityGroup = new aws.ec2.SecurityGroup("redis-sg", {
  vpcId: vpc.vpcId,
  description: "Security group for Redis cluster",
  ingress: [{
    protocol: "tcp",
    fromPort: 6379,
    toPort: 6379,
    cidrBlocks: [vpc.vpc.cidrBlock]
  }],
  egress: [{
    protocol: "-1",
    fromPort: 0,
    toPort: 0,
    cidrBlocks: ["0.0.0.0/0"],
  }]
});

const redisSubnetGroup = new aws.elasticache.SubnetGroup("redis-subnet-group", {
  subnetIds: vpc.privateSubnetIds
});

const redis = new aws.elasticache.ReplicationGroup("rate-limiter-redis", {
  replicationGroupId: "rate-limiter",
  description: "Redis for distributed rate limiting",
  engine: "redis",
  engineVersion: "7.0",
  nodeType: "cache.t3.micro",
  port: 6379,
  numCacheClusters: 2,
  automaticFailoverEnabled: true,
  multiAzEnabled: true,
  snapshotRetentionLimit: 1,
  snapshotWindow: "15:00-18:00",
  subnetGroupName: redisSubnetGroup.name,
  securityGroupIds: [redisSecurityGroup.id],
  atRestEncryptionEnabled: true,
  transitEncryptionEnabled: false
});

const lambdaRole = new aws.iam.Role("lambda-role", {
  assumeRolePolicy: aws.iam.assumeRolePolicyForPrincipal({
    Service: "lambda.amazonaws.com"
  })
});

new aws.iam.RolePolicyAttachment("lambda-basic-execution", {
  role: lambdaRole.name,
  policyArn: aws.iam.ManagedPolicy.AWSLambdaVPCAccessExecutionRole
});

const lambdaSecurityGroup = new aws.ec2.SecurityGroup("lambda-sg", {
  vpcId: vpc.vpcId,
  description: "Security group for Lambda functions",
  egress: [{
    protocol: "-1",
    fromPort: 0,
    toPort: 0,
    cidrBlocks: ["0.0.0.0/0"]
  }]
});

const wafWebAcl = new aws.wafv2.WebAcl("rate-limiter-waf", {
  scope: "CLOUDFRONT",
  description: "WAF for rate limiting with IP and geo-based rules",
  defaultAction: {
    allow: {}
  },
  rules: [
    {
      name: "IPRateLimit",
      priority: 1,
      statement: {
        rateBasedStatement: {
          limit: 100,
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
    },
    {
      name: "GeoBlocking",
      priority: 2,
      statement: {
        geoMatchStatement: {
          countryCodes: ["KR"]
        }
      },
      action: {
        block: {}
      },
      visibilityConfig: {
        sampledRequestsEnabled: true,
        cloudwatchMetricsEnabled: true,
        metricName: "GeoBlocking"
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
  description: "Rate Limiter API with custom authorizer",
  endpointConfiguration: {
    types: "REGIONAL"
  }
});

/*
 * API Gateway가 기본 제공하는 토큰 버킷 알고리즘 활용 방식:
 *
 * const usagePlan = new aws.apigateway.UsagePlan("rate-limiter-usage-plan", {
 *   description: "Token Bucket based rate limiting",
 *   throttleSettings: {
 *     rateLimit: 100,      // 초당 평균 요청 수 (steady-state rate)
 *     burstLimit: 200      // 버스트 용량 (토큰 버킷 크기)
 *   },
 *   quotaSettings: {
 *     limit: 10000,        // 일일 요청 할당량
 *     period: "DAY"
 *   },
 *   apiStages: [{
 *     apiId: api.id,
 *     stage: stage.stageName
 *   }]
 * }, { dependsOn: [stage] });
 *
 * API Key 생성 후 Usage Plan 연결:
 *
 * const apiKey = new aws.apigateway.ApiKey("rate-limiter-api-key", {
 *   description: "API key for rate limiting"
 * });
 *
 * new aws.apigateway.UsagePlanKey("usage-plan-key", {
 *   keyId: apiKey.id,
 *   keyType: "API_KEY",
 *   usagePlanId: usagePlan.id
 * });
 *
 * API Key 관련 설정 방법:
 * - method 생성 시 apiKeyRequired: true 옵션 추가
 * - 요청 시 x-api-key 헤더에 API Key 포함 필요
 */

// Lambda 함수 빌드 자동화
const lambdaBuild = new command.local.Command("lambda-build", {
  create: "cd lambda && npm install && npm run build",
  assetPaths: ["lambda/dist"]
});

const lambdaLayer = new aws.lambda.LayerVersion("redis-layer", {
  compatibleRuntimes: ["nodejs20.x"],
  code: new pulumi.asset.FileArchive("./lambda/layer"),
  layerName: "redis-layer"
});

const authorizerFunction = new aws.lambda.Function("rate-limiter-authorizer", {
  runtime: "nodejs20.x",
  role: lambdaRole.arn,
  handler: "dist/authorizer.handler",
  layers: [lambdaLayer.arn],
  code: new pulumi.asset.FileArchive("./lambda"),
  environment: {
    variables: {
      REDIS_ENDPOINT: redis.configurationEndpointAddress
    }
  },
  vpcConfig: {
    subnetIds: vpc.privateSubnetIds,
    securityGroupIds: [lambdaSecurityGroup.id]
  },
  timeout: 30
}, { dependsOn: [lambdaBuild] });

const authorizer = new aws.apigateway.Authorizer("rate-limiter-authorizer", {
  restApi: api.id,
  authorizerUri: authorizerFunction.invokeArn,
  authorizerResultTtlInSeconds: 0,
  identitySource: "method.request.header.Authorization",
  type: "REQUEST"
});

new aws.lambda.Permission("authorizer-permission", {
  action: "lambda:InvokeFunction",
  function: authorizerFunction.name,
  principal: "apigateway.amazonaws.com",
  sourceArn: api.executionArn.apply(arn => `${arn}/*`)
});

const backendFunction = new aws.lambda.Function("rate-limiter-backend", {
  runtime: "nodejs20.x",
  role: lambdaRole.arn,
  handler: "dist/backend.handler",
  code: new pulumi.asset.FileArchive("./lambda")
}, { dependsOn: [lambdaBuild] });

const resource = new aws.apigateway.Resource("api-resource", {
  restApi: api.id,
  parentId: api.rootResourceId,
  pathPart: "check"
});

const method = new aws.apigateway.Method("api-method", {
  restApi: api.id,
  resourceId: resource.id,
  httpMethod: "GET",
  authorization: "CUSTOM",
  authorizerId: authorizer.id
});

const integration = new aws.apigateway.Integration("api-integration", {
  restApi: api.id,
  resourceId: resource.id,
  httpMethod: method.httpMethod,
  integrationHttpMethod: "POST",
  type: "AWS_PROXY",
  uri: backendFunction.invokeArn
});

new aws.lambda.Permission("backend-permission", {
  action: "lambda:InvokeFunction",
  function: backendFunction.name,
  principal: "apigateway.amazonaws.com",
  sourceArn: api.executionArn.apply(arn => `${arn}/*/*`)
});

const deployment = new aws.apigateway.Deployment("api-deployment", {
  restApi: api.id,
  triggers: {
    redeployment: JSON.stringify([method, integration])
  }
}, {
  dependsOn: [method, integration]
});

const stage = new aws.apigateway.Stage("api-stage", {
  restApi: api.id,
  deployment: deployment.id,
  stageName: "prod"
});

const distribution = new aws.cloudfront.Distribution("rate-limiter-cdn", {
  enabled: true,
  comment: "Rate Limiter CDN with geo-restriction",
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
      headers: ["Authorization"],
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
  period: 60,
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
export const apiGatewayUrl = stage.invokeUrl;
export const redisEndpoint = redis.configurationEndpointAddress;
