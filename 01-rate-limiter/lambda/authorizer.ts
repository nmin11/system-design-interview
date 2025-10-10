import { APIGatewayRequestAuthorizerEvent, APIGatewayAuthorizerResult } from 'aws-lambda';
import { createClient } from 'redis';

const redisClient = createClient({
  socket: {
    host: process.env.REDIS_ENDPOINT!,
    port: 6379
  }
});

redisClient.connect().catch(console.error);

export const handler = async (event: APIGatewayRequestAuthorizerEvent): Promise<APIGatewayAuthorizerResult> => {
  const ip = event.requestContext.identity.sourceIp;
  const windowSize = 60; // 60 seconds
  const maxRequests = 100; // 100 requests per minute
  const now = Date.now();
  const windowStart = now - (windowSize * 1000);

  const key = `rate:${ip}`;

  try {
    // Remove old entries outside the sliding window
    await redisClient.zRemRangeByScore(key, 0, windowStart);

    // Count requests in current window
    const requestCount = await redisClient.zCard(key);

    if (requestCount >= maxRequests) {
      return {
        principalId: ip,
        policyDocument: {
          Version: '2012-10-17',
          Statement: [{
            Action: 'execute-api:Invoke',
            Effect: 'Deny',
            Resource: event.methodArn
          }]
        },
        context: {
          rateLimitExceeded: 'true',
          remainingRequests: '0'
        }
      };
    }

    // Add current request to the window
    await redisClient.zAdd(key, { score: now, value: `${now}` });
    await redisClient.expire(key, windowSize);

    return {
      principalId: ip,
      policyDocument: {
        Version: '2012-10-17',
        Statement: [{
          Action: 'execute-api:Invoke',
          Effect: 'Allow',
          Resource: event.methodArn
        }]
      },
      context: {
        rateLimitExceeded: 'false',
        remainingRequests: String(maxRequests - requestCount - 1)
      }
    };
  } catch (error) {
    console.error('Redis error:', error);
    // Fail open in case of Redis errors
    return {
      principalId: ip,
      policyDocument: {
        Version: '2012-10-17',
        Statement: [{
          Action: 'execute-api:Invoke',
          Effect: 'Allow',
          Resource: event.methodArn
        }]
      }
    };
  }
};
