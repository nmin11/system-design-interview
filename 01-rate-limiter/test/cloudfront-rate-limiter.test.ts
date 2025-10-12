import { loadConfig } from './config';
import {
  makeConcurrentRequests,
  aggregateResults,
  printTestResults,
  makeSequentialRequests,
} from './utils';

describe('CloudFront WAF IP-based Rate Limiting Tests', () => {
  const config = loadConfig();
  const cloudfrontApiUrl = config.cloudfrontApiUrl!;
  const wafRateLimit = config.wafRateLimit;

  // 기본 테스트
  describe('CloudFront Basic Functionality', () => {
    test('should forward requests to API Gateway when within WAF limit', async () => {
      const results = await makeConcurrentRequests(
        cloudfrontApiUrl,
        config.apiKey,
        10
      );

      const aggregated = aggregateResults(results);
      printTestResults(results);

      expect(aggregated.success).toBe(10);
      expect(aggregated.wafBlocked).toBe(0);
    });
  });

  // WAF IP 제한 관련 테스트
  describe('IP-based Rate Limiting (WAF)', () => {
    test('should allow requests within WAF IP rate limit', async () => {
      const withinLimit = Math.floor(wafRateLimit * 0.5);
      console.log(`\n[Test] Sending ${withinLimit} requests within WAF limit...`);

      const results = await makeConcurrentRequests(
        cloudfrontApiUrl,
        config.apiKey,
        withinLimit
      );

      const aggregated = aggregateResults(results);
      printTestResults(results);

      expect(aggregated.total).toBe(withinLimit);
      expect(aggregated.success).toBeGreaterThan(withinLimit * 0.75);
      expect(aggregated.wafBlocked).toBe(0);
    }, 30000);

    test('should block requests exceeding WAF IP rate limit', async () => {
      const exceedLimit = wafRateLimit * 2;
      console.log(`\n[Test] Sending ${exceedLimit} requests to exceed WAF limit...`);

      const results = await makeSequentialRequests(
        cloudfrontApiUrl,
        config.apiKey,
        exceedLimit
      );

      const aggregated = aggregateResults(results);
      printTestResults(results, true);

      expect(aggregated.total).toBe(exceedLimit);
      expect(aggregated.wafBlocked).toBeGreaterThan(0);

      const blockRate = (aggregated.wafBlocked / exceedLimit) * 100;
      console.log(`\nWAF blocked ${aggregated.wafBlocked} requests (${blockRate.toFixed(2)}%)`);
    }, 60000);
  });
});
