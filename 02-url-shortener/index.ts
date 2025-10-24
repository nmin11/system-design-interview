import * as pulumi from "@pulumi/pulumi";
import * as aws from "@pulumi/aws";

const config = new pulumi.Config();
const mongodbUri = config.requireSecret("mongodbUri");

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

export const functionUrlEndpoint = functionUrl.functionUrl;
