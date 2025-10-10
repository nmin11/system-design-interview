import { APIGatewayProxyEvent, APIGatewayProxyResult } from 'aws-lambda';

export const handler = async (event: APIGatewayProxyEvent): Promise<APIGatewayProxyResult> => {
  return {
    statusCode: 200,
    headers: {
      'Content-Type': 'application/json',
      'X-RateLimit-Remaining': event.requestContext.authorizer?.remainingRequests || '0'
    },
    body: JSON.stringify({
      message: 'Request successful',
      ip: event.requestContext.identity.sourceIp,
      timestamp: new Date().toISOString()
    })
  };
};
