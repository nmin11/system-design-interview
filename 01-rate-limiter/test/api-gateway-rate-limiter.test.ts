import { loadConfig } from './config';
import {
  makeConcurrentRequests,
  makeSequentialRequests,
  aggregateResults,
  parseRateLimitHeaders,
  sleep,
  printTestResults,
} from './utils';

describe('Rate Limiter Integration Tests', () => {
  const config = loadConfig();
  const apiUrl = `${config.apiUrl}/check`;

  // 기본 테스트
  describe('Basic Functionality', () => {
    test('should return 200 for a single request', async () => {
      const results = await makeConcurrentRequests(apiUrl, config.apiKey, 1);

      expect(results).toHaveLength(1);
      expect(results[0].status).toBe(200);
    });

    test('should include rate limit headers in response', async () => {
      const results = await makeConcurrentRequests(apiUrl, config.apiKey, 1);
      const headers = parseRateLimitHeaders(results[0].headers);

      expect(headers.limit).toBeDefined();
      expect(headers.burst).toBeDefined();
    });
  });

  // Burst Limit 관련 테스트
  describe('Burst Limit Behavior', () => {
    afterEach(async () => {
      await sleep(10000);
    });

    test('should allow requests within burst limit', async () => {
      const withinBurst = Math.floor(config.burstLimit * 0.8);
      const results = await makeConcurrentRequests(
        apiUrl,
        config.apiKey,
        withinBurst
      );

      const aggregated = aggregateResults(results);
      printTestResults(results);

      expect(aggregated.success).toBe(withinBurst);
      expect(aggregated.rateLimited).toBe(0);
    });

    test('should reject requests exceeding burst limit', async () => {
      const exceedBurst = config.burstLimit + 1000;
      const results = await makeConcurrentRequests(
        apiUrl,
        config.apiKey,
        exceedBurst
      );

      const aggregated = aggregateResults(results);
      printTestResults(results, true);

      expect(aggregated.total).toBe(exceedBurst);
      expect(aggregated.rateLimited).toBeGreaterThan(0);
    });

    test('should handle exactly burst limit concurrent requests', async () => {
      const results = await makeConcurrentRequests(
        apiUrl,
        config.apiKey,
        config.burstLimit
      );

      const aggregated = aggregateResults(results);
      printTestResults(results);

      expect(aggregated.total).toBe(config.burstLimit);
      expect(aggregated.success).toEqual(config.burstLimit);
    });
  });

  // 토큰 리필 테스트
  describe('Token Bucket Algorithm', () => {
    test('should refill tokens over time', async () => {
      console.log('\n[Phase 1] Sending burst requests...');
      const burst1 = await makeConcurrentRequests(
        apiUrl,
        config.apiKey,
        config.burstLimit
      );
      const agg1 = aggregateResults(burst1);
      printTestResults(burst1);

      console.log('[Phase 2] Waiting for token refill (2 seconds)...');
      await sleep(2000);

      console.log('[Phase 3] Sending requests after refill...');
      const burst2 = await makeSequentialRequests(
        apiUrl,
        config.apiKey,
        config.rateLimit,
        10
      );
      const agg2 = aggregateResults(burst2);
      printTestResults(burst2, true);

      expect(agg2.success).toBeGreaterThan(0);
    }, 30000);
  });

  // API Key 에러 테스트
  describe('Error Handling', () => {
    test('should handle invalid API key', async () => {
      const results = await makeConcurrentRequests(
        apiUrl,
        'invalid-key',
        1
      );

      expect(results[0].status).toBe(403);
    });
  });
});
