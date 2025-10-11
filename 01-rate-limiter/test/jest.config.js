module.exports = {
  preset: 'ts-jest',
  testEnvironment: 'node',
  testMatch: ['**/*.test.ts'],
  collectCoverageFrom: [
    '**/*.ts',
    '!**/*.d.ts',
    '!**/node_modules/**'
  ],
  moduleFileExtensions: ['ts', 'js', 'json'],
  verbose: true,
  testTimeout: 30000
};
