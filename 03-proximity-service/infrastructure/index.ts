import * as pulumi from "@pulumi/pulumi";
import * as aws from "@pulumi/aws";
import * as awsx from "@pulumi/awsx";

const config = new pulumi.Config();
const projectName = "proximity-service";

// VPC
const vpc = new awsx.ec2.Vpc(`${projectName}-vpc`, {
  cidrBlock: "10.0.0.0/16",
  numberOfAvailabilityZones: 2,
  natGateways: { strategy: "Single" },
  tags: { Name: `${projectName}-vpc` },
});

// Security Groups
const albSecurityGroup = new aws.ec2.SecurityGroup(`${projectName}-alb-sg`, {
  vpcId: vpc.vpcId,
  description: "Security group for ALB",
  ingress: [
    { protocol: "tcp", fromPort: 80, toPort: 80, cidrBlocks: ["0.0.0.0/0"] },
    { protocol: "tcp", fromPort: 443, toPort: 443, cidrBlocks: ["0.0.0.0/0"] },
  ],
  egress: [
    { protocol: "-1", fromPort: 0, toPort: 0, cidrBlocks: ["0.0.0.0/0"] },
  ],
  tags: { Name: `${projectName}-alb-sg` },
});

const ecsSecurityGroup = new aws.ec2.SecurityGroup(`${projectName}-ecs-sg`, {
  vpcId: vpc.vpcId,
  description: "Security group for ECS tasks",
  ingress: [
    {
      protocol: "tcp",
      fromPort: 8080,
      toPort: 8081,
      securityGroups: [albSecurityGroup.id],
    },
  ],
  egress: [
    { protocol: "-1", fromPort: 0, toPort: 0, cidrBlocks: ["0.0.0.0/0"] },
  ],
  tags: { Name: `${projectName}-ecs-sg` },
});

const rdsSecurityGroup = new aws.ec2.SecurityGroup(`${projectName}-rds-sg`, {
  vpcId: vpc.vpcId,
  description: "Security group for RDS",
  ingress: [
    {
      protocol: "tcp",
      fromPort: 5432,
      toPort: 5432,
      securityGroups: [ecsSecurityGroup.id],
    },
  ],
  egress: [
    { protocol: "-1", fromPort: 0, toPort: 0, cidrBlocks: ["0.0.0.0/0"] },
  ],
  tags: { Name: `${projectName}-rds-sg` },
});

// ECS Cluster
const cluster = new aws.ecs.Cluster(`${projectName}-cluster`, {
  name: `${projectName}-cluster`,
  settings: [{ name: "containerInsights", value: "enabled" }],
  tags: { Name: `${projectName}-cluster` },
});

// ALB
const alb = new aws.lb.LoadBalancer(`${projectName}-alb`, {
  loadBalancerType: "application",
  securityGroups: [albSecurityGroup.id],
  subnets: vpc.publicSubnetIds,
  tags: { Name: `${projectName}-alb` },
});

// LBS Target Group
const lbsTargetGroup = new aws.lb.TargetGroup("prox-lbs-tg", {
  port: 8081,
  protocol: "HTTP",
  targetType: "ip",
  vpcId: vpc.vpcId,
  healthCheck: {
    path: "/actuator/health",
    healthyThreshold: 2,
    unhealthyThreshold: 3,
    timeout: 5,
    interval: 30,
  },
  tags: { Name: `${projectName}-lbs-tg` },
});

// Place Service Target Group
const placeTargetGroup = new aws.lb.TargetGroup("prox-place-tg", {
  port: 8080,
  protocol: "HTTP",
  targetType: "ip",
  vpcId: vpc.vpcId,
  healthCheck: {
    path: "/actuator/health",
    healthyThreshold: 2,
    unhealthyThreshold: 3,
    timeout: 5,
    interval: 30,
  },
  tags: { Name: `${projectName}-place-tg` },
});

// ALB Listener
const listener = new aws.lb.Listener(`${projectName}-listener`, {
  loadBalancerArn: alb.arn,
  port: 80,
  protocol: "HTTP",
  defaultActions: [
    {
      type: "fixed-response",
      fixedResponse: {
        contentType: "text/plain",
        messageBody: "Not Found",
        statusCode: "404",
      },
    },
  ],
});

// Listener Rules
new aws.lb.ListenerRule(`${projectName}-lbs-rule`, {
  listenerArn: listener.arn,
  priority: 10,
  conditions: [{ pathPattern: { values: ["/nearby*", "/nearest*", "/search*"] } }],
  actions: [{ type: "forward", targetGroupArn: lbsTargetGroup.arn }],
});

new aws.lb.ListenerRule(`${projectName}-place-rule`, {
  listenerArn: listener.arn,
  priority: 20,
  conditions: [{ pathPattern: { values: ["/places*"] } }],
  actions: [{ type: "forward", targetGroupArn: placeTargetGroup.arn }],
});

// ECR Repositories
const services = ["lbs", "place-service"] as const;

const repositories = services.map((service) => {
  const repo = new aws.ecr.Repository(`${projectName}-${service}`, {
    name: `${projectName}/${service}`,
    imageTagMutability: "MUTABLE",
    imageScanningConfiguration: {
      scanOnPush: true,
    },
    forceDelete: true,
  });

  // Lifecycle policy: 최근 10개 이미지만 유지
  new aws.ecr.LifecyclePolicy(`${projectName}-${service}-lifecycle`, {
    repository: repo.name,
    policy: JSON.stringify({
      rules: [
        {
          rulePriority: 1,
          description: "Keep only last 10 images",
          selection: {
            tagStatus: "any",
            countType: "imageCountMoreThan",
            countNumber: 10,
          },
          action: {
            type: "expire",
          },
        },
      ],
    }),
  });

  return { service, repo };
});

// RDS (PostgreSQL with PostGIS)
const dbSubnetGroup = new aws.rds.SubnetGroup(`${projectName}-db-subnet`, {
  subnetIds: vpc.privateSubnetIds,
  tags: { Name: `${projectName}-db-subnet` },
});

const dbPassword = config.requireSecret("dbPassword");

const database = new aws.rds.Instance(`${projectName}-db`, {
  identifier: `${projectName}-db`,
  engine: "postgres",
  engineVersion: "15",
  instanceClass: "db.t3.micro",
  allocatedStorage: 20,
  storageType: "gp2",
  dbName: "proximity_db",
  username: "proximity",
  password: dbPassword,
  dbSubnetGroupName: dbSubnetGroup.name,
  vpcSecurityGroupIds: [rdsSecurityGroup.id],
  skipFinalSnapshot: true,
  publiclyAccessible: false,
  tags: { Name: `${projectName}-db` },
});

// ECS Task Execution Role
const taskExecutionRole = new aws.iam.Role(`${projectName}-task-exec-role`, {
  assumeRolePolicy: JSON.stringify({
    Version: "2012-10-17",
    Statement: [
      {
        Action: "sts:AssumeRole",
        Effect: "Allow",
        Principal: { Service: "ecs-tasks.amazonaws.com" },
      },
    ],
  }),
  tags: { Name: `${projectName}-task-exec-role` },
});

new aws.iam.RolePolicyAttachment(`${projectName}-task-exec-policy`, {
  role: taskExecutionRole.name,
  policyArn: "arn:aws:iam::aws:policy/service-role/AmazonECSTaskExecutionRolePolicy",
});

// ECS Task Definitions & Services
const lbsRepo = repositories.find((r) => r.service === "lbs")!.repo;
const placeRepo = repositories.find((r) => r.service === "place-service")!.repo;

// LBS Task Definition
const lbsTaskDefinition = new aws.ecs.TaskDefinition(`${projectName}-lbs-task`, {
  family: `${projectName}-lbs`,
  networkMode: "awsvpc",
  requiresCompatibilities: ["FARGATE"],
  cpu: "256",
  memory: "512",
  executionRoleArn: taskExecutionRole.arn,
  containerDefinitions: pulumi
    .all([lbsRepo.repositoryUrl, database.endpoint, dbPassword])
    .apply(([repoUrl, dbEndpoint, password]) =>
      JSON.stringify([
        {
          name: "lbs",
          image: `${repoUrl}:latest`,
          essential: true,
          portMappings: [{ containerPort: 8081, protocol: "tcp" }],
          environment: [
            { name: "SPRING_DATASOURCE_URL", value: `jdbc:postgresql://${dbEndpoint}/proximity_db` },
            { name: "SPRING_DATASOURCE_USERNAME", value: "proximity" },
            { name: "SPRING_DATASOURCE_PASSWORD", value: password },
          ],
          logConfiguration: {
            logDriver: "awslogs",
            options: {
              "awslogs-group": `/ecs/${projectName}-lbs`,
              "awslogs-region": aws.getRegionOutput().name,
              "awslogs-stream-prefix": "ecs",
              "awslogs-create-group": "true",
            },
          },
        },
      ])
    ),
  tags: { Name: `${projectName}-lbs-task` },
});

// Place Service Task Definition
const placeTaskDefinition = new aws.ecs.TaskDefinition(`${projectName}-place-task`, {
  family: `${projectName}-place`,
  networkMode: "awsvpc",
  requiresCompatibilities: ["FARGATE"],
  cpu: "256",
  memory: "512",
  executionRoleArn: taskExecutionRole.arn,
  containerDefinitions: pulumi
    .all([placeRepo.repositoryUrl, database.endpoint, dbPassword])
    .apply(([repoUrl, dbEndpoint, password]) =>
      JSON.stringify([
        {
          name: "place-service",
          image: `${repoUrl}:latest`,
          essential: true,
          portMappings: [{ containerPort: 8080, protocol: "tcp" }],
          environment: [
            { name: "SPRING_DATASOURCE_URL", value: `jdbc:postgresql://${dbEndpoint}/proximity_db` },
            { name: "SPRING_DATASOURCE_USERNAME", value: "proximity" },
            { name: "SPRING_DATASOURCE_PASSWORD", value: password },
          ],
          logConfiguration: {
            logDriver: "awslogs",
            options: {
              "awslogs-group": `/ecs/${projectName}-place`,
              "awslogs-region": aws.getRegionOutput().name,
              "awslogs-stream-prefix": "ecs",
              "awslogs-create-group": "true",
            },
          },
        },
      ])
    ),
  tags: { Name: `${projectName}-place-task` },
});

// LBS Service
const lbsService = new aws.ecs.Service(`${projectName}-lbs-service`, {
  name: `${projectName}-lbs`,
  cluster: cluster.arn,
  taskDefinition: lbsTaskDefinition.arn,
  desiredCount: 1,
  launchType: "FARGATE",
  networkConfiguration: {
    subnets: vpc.privateSubnetIds,
    securityGroups: [ecsSecurityGroup.id],
    assignPublicIp: false,
  },
  loadBalancers: [
    {
      targetGroupArn: lbsTargetGroup.arn,
      containerName: "lbs",
      containerPort: 8081,
    },
  ],
  tags: { Name: `${projectName}-lbs-service` },
});

// Place Service
const placeService = new aws.ecs.Service(`${projectName}-place-service`, {
  name: `${projectName}-place`,
  cluster: cluster.arn,
  taskDefinition: placeTaskDefinition.arn,
  desiredCount: 1,
  launchType: "FARGATE",
  networkConfiguration: {
    subnets: vpc.privateSubnetIds,
    securityGroups: [ecsSecurityGroup.id],
    assignPublicIp: false,
  },
  loadBalancers: [
    {
      targetGroupArn: placeTargetGroup.arn,
      containerName: "place-service",
      containerPort: 8080,
    },
  ],
  tags: { Name: `${projectName}-place-service` },
});

// Exports
export const repositoryUrls = Object.fromEntries(
  repositories.map(({ service, repo }) => [service, repo.repositoryUrl])
);

export const vpcId = vpc.vpcId;
export const albDnsName = alb.dnsName;
export const albUrl = pulumi.interpolate`http://${alb.dnsName}`;
export const databaseEndpoint = database.endpoint;
