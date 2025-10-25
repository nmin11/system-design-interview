import * as pulumi from "@pulumi/pulumi";
import * as aws from "@pulumi/aws";

const config = new pulumi.Config();
const mongodbUri = config.requireSecret("mongodbUri");
const domainName = "nmin11.xyz";
const enableCustomDomain = config.getBoolean("enableCustomDomain") ?? false;

const lambdaRole = new aws.iam.Role("urlShortenerLambdaRole", {
  assumeRolePolicy: JSON.stringify({
    Version: "2012-10-17",
    Statement: [{
      Action: "sts:AssumeRole",
      Principal: {
        Service: "lambda.amazonaws.com",
      },
      Effect: "Allow",
    }],
  }),
});

new aws.iam.RolePolicyAttachment("lambdaBasicExecution", {
  role: lambdaRole.name,
  policyArn: "arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole",
});

const urlShortenerLambda = new aws.lambda.Function("urlShortenerFunction", {
  runtime: "provided.al2023",
  handler: "bootstrap",
  role: lambdaRole.arn,
  code: new pulumi.asset.FileArchive("./lambda/bootstrap.zip"),
  timeout: 30,
  memorySize: 256,
  environment: {
    variables: {
      MONGODB_URI: mongodbUri,
    },
  },
});

const functionUrl = new aws.lambda.FunctionUrl("urlShortenerFunctionUrl", {
  functionName: urlShortenerLambda.name,
  authorizationType: "NONE",
  cors: {
    allowOrigins: ["*"],
    allowMethods: ["GET", "POST"],
    allowHeaders: ["content-type"],
    maxAge: 300,
  },
});

const hostedZone = new aws.route53.Zone("urlShortenerZone", {
  name: domainName,
});

// Custom Domain Setup (conditional)
let certificate: aws.acm.Certificate | undefined;
let distribution: aws.cloudfront.Distribution | undefined;

if (enableCustomDomain) {
  const usEast1Provider = new aws.Provider("usEast1Provider", {
    region: "us-east-1",
  });

  certificate = new aws.acm.Certificate("urlShortenerCert", {
    domainName: domainName,
    validationMethod: "DNS",
  }, { provider: usEast1Provider });

  const certValidationRecords = certificate.domainValidationOptions.apply(options => {
    return options.map((option, index) => {
      return new aws.route53.Record(`certValidation-${index}`, {
        name: option.resourceRecordName,
        type: option.resourceRecordType,
        records: [option.resourceRecordValue],
        zoneId: hostedZone.zoneId,
        ttl: 60,
      });
    });
  });

  const certValidation = new aws.acm.CertificateValidation("certValidation", {
    certificateArn: certificate.arn,
    validationRecordFqdns: certificate.domainValidationOptions.apply(options =>
      options.map(option => option.resourceRecordName)
    ),
  }, { provider: usEast1Provider });

  distribution = new aws.cloudfront.Distribution("urlShortenerDistribution", {
    enabled: true,
    aliases: [domainName],
    origins: [{
      originId: "lambda-function-url",
      domainName: functionUrl.functionUrl.apply(url => url.replace("https://", "").replace("/", "")),
      customOriginConfig: {
        httpPort: 80,
        httpsPort: 443,
        originProtocolPolicy: "https-only",
        originSslProtocols: ["TLSv1.2"],
      },
    }],
    defaultCacheBehavior: {
      targetOriginId: "lambda-function-url",
      viewerProtocolPolicy: "redirect-to-https",
      allowedMethods: ["GET", "HEAD", "OPTIONS", "PUT", "POST", "PATCH", "DELETE"],
      cachedMethods: ["GET", "HEAD", "OPTIONS"],
      forwardedValues: {
        queryString: true,
        headers: ["Accept", "Accept-Language", "Authorization"],
        cookies: {
          forward: "all",
        },
      },
      minTtl: 0,
      defaultTtl: 60,
      maxTtl: 3600,
      compress: true,
    },
    restrictions: {
      geoRestriction: {
        restrictionType: "none",
      },
    },
    viewerCertificate: {
      acmCertificateArn: certValidation.certificateArn,
      sslSupportMethod: "sni-only",
      minimumProtocolVersion: "TLSv1.2_2021",
    },
  });

  new aws.route53.Record("urlShortenerARecord", {
    zoneId: hostedZone.zoneId,
    name: domainName,
    type: "A",
    aliases: [{
      name: distribution.domainName,
      zoneId: distribution.hostedZoneId,
      evaluateTargetHealth: false,
    }],
  });
}

export const functionUrlEndpoint = functionUrl.functionUrl;
export const nameServers = hostedZone.nameServers;
export const cloudFrontDomain = distribution?.domainName;
export const customDomain = enableCustomDomain
  ? pulumi.interpolate`https://${domainName}`
  : undefined;
