import axios, { AxiosError, AxiosResponse } from 'axios';

export interface RequestResult {
  status: number;
  headers: Record<string, string>;
  timestamp: number;
}

export async function makeRequest(
  url: string,
  apiKey: string
): Promise<RequestResult> {
  const timestamp = Date.now();

  try {
    const response: AxiosResponse = await axios.get(url, {
      headers: {
        'x-api-key': apiKey,
      },
      validateStatus: () => true
    });

    return {
      status: response.status,
      headers: response.headers as Record<string, string>,
      timestamp
    };
  } catch (error) {
    const axiosError = error as AxiosError;
    return {
      status: axiosError.response?.status || 500,
      headers: (axiosError.response?.headers as Record<string, string>) || {},
      timestamp
    };
  }
}

export async function makeConcurrentRequests(
  url: string,
  apiKey: string,
  count: number
): Promise<RequestResult[]> {
  const requests = Array(count)
    .fill(null)
    .map(() => makeRequest(url, apiKey));

  return Promise.all(requests);
}

export async function makeSequentialRequests(
  url: string,
  apiKey: string,
  count: number,
  intervalMs: number = 0
): Promise<RequestResult[]> {
  const results: RequestResult[] = [];

  for (let i = 0; i < count; i++) {
    const result = await makeRequest(url, apiKey);
    results.push(result);

    if (intervalMs > 0 && i < count - 1) {
      await sleep(intervalMs);
    }
  }

  return results;
}

export function aggregateResults(results: RequestResult[]): {
  total: number;
  success: number;
  rateLimited: number;
  errors: number;
  byStatus: Record<number, number>;
} {
  const byStatus: Record<number, number> = {};

  results.forEach(result => {
    byStatus[result.status] = (byStatus[result.status] || 0) + 1;
  });

  return {
    total: results.length,
    success: byStatus[200] || 0,
    rateLimited: byStatus[429] || 0,
    errors: Object.entries(byStatus)
      .filter(([status]) => parseInt(status) >= 400 && parseInt(status) !== 429)
      .reduce((sum, [, count]) => sum + count, 0),
    byStatus
  };
}

export function parseRateLimitHeaders(headers: Record<string, string>): {
  limit?: number;
  burst?: number;
  quota?: number;
} {
  return {
    limit: headers['x-ratelimit-limit']
      ? parseInt(headers['x-ratelimit-limit'], 10)
      : undefined,
    burst: headers['x-ratelimit-burst']
      ? parseInt(headers['x-ratelimit-burst'], 10)
      : undefined,
    quota: headers['x-ratelimit-quota']
      ? parseInt(headers['x-ratelimit-quota'], 10)
      : undefined
  };
}

export function sleep(ms: number): Promise<void> {
  return new Promise(resolve => setTimeout(resolve, ms));
}

export function printTestResults(results: RequestResult[], showDebug: boolean = false): void {
  const aggregated = aggregateResults(results);

  console.log('\n=== Test Results ===');
  console.log(`Total Requests: ${aggregated.total}`);
  console.log(`Success (200): ${aggregated.success}`);
  console.log(`Rate Limited (429): ${aggregated.rateLimited}`);
  console.log(`Errors: ${aggregated.errors}`);
  console.log('\nBy Status Code:');

  Object.entries(aggregated.byStatus)
    .sort(([a], [b]) => parseInt(a) - parseInt(b))
    .forEach(([status, count]) => {
      console.log(`  ${status}: ${count}`);
    });

  if (showDebug) {
    console.log('\n[DEBUG] First 10 request status codes:');
    results.slice(0, 10).forEach((result, idx) => {
      console.log(`  Request ${idx + 1}: ${result.status}`);
    });

    const rateLimitedIndexes = results
      .map((r, idx) => r.status === 429 ? idx : -1)
      .filter(idx => idx !== -1);

    if (rateLimitedIndexes.length > 0) {
      console.log(`\n[DEBUG] Rate limited request indexes (first 10): ${rateLimitedIndexes.slice(0, 10).join(', ')}`);
    }
  }

  console.log('==================\n');
}
