import { config } from 'dotenv';
import { resolve } from 'path';

// Load .env file from the test directory
config({ path: resolve(__dirname, '.env') });

export interface TestConfig {
  apiUrl: string;
  apiKey: string;
  cloudfrontUrl?: string;
  rateLimit: number;
  burstLimit: number;
  quotaLimit: number;
}

export function loadConfig(): TestConfig {
  const apiUrl = process.env.API_URL;
  const apiKey = process.env.API_KEY;

  if (!apiUrl) {
    throw new Error('API_URL 환경 변수가 설정되지 않았습니다.');
  }

  if (!apiKey) {
    throw new Error('API_KEY 환경 변수가 설정되지 않았습니다.');
  }

  return {
    apiUrl,
    apiKey,
    cloudfrontUrl: process.env.CLOUDFRONT_URL,
    rateLimit: parseInt(process.env.RATE_LIMIT || '100', 10),
    burstLimit: parseInt(process.env.BURST_LIMIT || '200', 10),
    quotaLimit: parseInt(process.env.QUOTA_LIMIT || '20000', 10),
  };
}
