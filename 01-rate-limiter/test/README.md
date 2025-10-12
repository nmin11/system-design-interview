# Rate Limiter Integration Tests

Jest + Axios 기반의 Rate Limiter 통합 테스트입니다.

## 설치

```bash
cd test
npm install
```

## 설정

1. `.env.example`을 복사하여 `.env` 파일을 생성합니다:
    ```bash
    cp .env.example .env
    ```

2. Pulumi stack output으로 필요한 값들을 가져옵니다:
    ```bash
    # 상위 디렉토리에서
    cd ..
    pulumi stack output apiUrl
    pulumi stack output apiKeyId
    pulumi stack output cloudfrontUrl
    ```

3. `.env` 파일에 값들을 설정합니다:
   - `API_URL`: API Gateway URL (API Gateway 직접 테스트용)
   - `API_KEY`: API Key 값 (AWS Console에서 확인)
   - `CLOUDFRONT_API_URL`: CloudFront URL (CloudFront + WAF 테스트용)
   - `WAF_RATE_LIMIT`: WAF IP-based rate limit (기본값: 1000)

## 테스트 실행

```bash
# 환경 변수 설정 후 테스트 실행
export $(cat .env | xargs) && npm test

# 또는 직접 환경 변수 전달
API_URL="https://xxx.execute-api.ap-northeast-2.amazonaws.com/prod" \
API_KEY="your-api-key" \
npm test

# watch 모드
export $(cat .env | xargs) && npm run test:watch

# coverage
export $(cat .env | xargs) && npm run test:coverage

# verbose 모드
export $(cat .env | xargs) && npm run test:verbose
```

## 테스트 케이스

### API Gateway Rate Limiter (`api-gateway-rate-limiter.test.ts`)

API Gateway의 Usage Plan을 사용한 Token Bucket 알고리즘 기반 Rate Limiting 테스트입니다.

#### 1. Basic Functionality

- 단일 요청 성공 확인
- Rate limit 헤더 검증

#### 2. Burst Limit Behavior

- Burst limit 이하 요청 허용 확인
- Burst limit 초과 시 throttling 확인
- 정확히 burst limit만큼 요청 시 동작 확인

#### 3. Token Bucket Algorithm

- 시간 경과에 따른 토큰 리필 확인
- Burst 후 recovery 테스트

#### 4. Error Handling

- 잘못된 API Key 처리 확인


### CloudFront WAF Rate Limiter (`cloudfront-rate-limiter.test.ts`)

CloudFront + WAF를 사용한 IP 기반 Rate Limiting 테스트입니다.

#### 1. CloudFront Basic Functionality

- CloudFront를 통한 요청 전달 확인
- WAF limit 이내 요청 성공 확인

#### 2. IP-based Rate Limiting (WAF)

- WAF IP rate limit 이하 요청 허용 확인
- WAF IP rate limit 초과 시 차단 확인
- 차단률(block rate) 측정

## 테스트 구조

```
test/
├── jest.config.js                      # Jest 설정
├── tsconfig.json                       # TypeScript 설정
├── package.json                        # 의존성 관리
├── config.ts                           # 환경 변수 로드
├── utils.ts                            # 테스트 유틸리티 함수
├── api-gateway-rate-limiter.test.ts    # API Gateway 테스트 스위트
├── cloudfront-rate-limiter.test.ts     # CloudFront WAF 테스트 스위트
├── .env.example                        # 환경 변수 예시
└── README.md                           # 이 파일
```

## 주요 특징

### 정밀한 요청 제어

- `makeConcurrentRequests`: 정확히 N개의 동시 요청
- `makeSequentialRequests`: 지정된 간격으로 순차 요청

### 상세한 결과 분석

- 상태 코드별 집계
- 성공률 계산
- Rate limit 헤더 파싱

### 시각화

- 테스트 결과 콘솔 출력
- 성능 메트릭 표시

## 예상 결과

### API Gateway Rate Limiter

- **Burst limit 동시 요청**:
  - 성공: burst limit 개수
  - Rate Limited: 0개

- **Burst limit 초과 요청**:
  - 성공: burst limit 개수
  - Rate Limited: 초과분

- **토큰 리필 테스트**:
  - 2초 대기 후 rate limit만큼 토큰 추가


### CloudFront WAF Rate Limiter

- **WAF limit 이내 요청**:
  - 성공: 요청 전체
  - WAF Blocked: 0개

- **WAF limit 초과 요청**:
  - 성공: WAF limit까지
  - WAF Blocked: 초과분 (403 상태 코드)

## 문제 해결

### API Key 오류

```
Error: API_KEY 환경 변수가 설정되지 않았습니다.
```
→ `.env` 파일을 확인하고 환경 변수를 export하세요.

### 403 Forbidden

```
status: 403
```

→ API Key 값이 올바른지 AWS Console에서 확인하세요.

### 타임아웃

```
Timeout - Async callback was not invoked within the 30000 ms timeout
```

→ `jest.config.js`의 `testTimeout` 값을 늘리거나 네트워크를 확인하세요.

### 디버깅 용도의 명령어들

```sh
# Stage 설정 확인
aws apigateway get-stage --rest-api-id <apiId> --stage-name prod

# Usage Plan 조회
aws apigateway get-usage-plans

# 단일 API 테스트
curl -v "https://<apiId>.execute-api.ap-northeast-2.amazonaws.com/prod/check" \
  -H "x-api-key: <apiKeyValue>" 2>&1 | grep -i "< x-"

# 연속 요청 테스트
for i in {1..5}; do
  echo "Request $i:"
  curl -s -o /dev/null -w "Status: %{http_code}\n" \
    -H "x-api-key: <apiKeyValue>" \
    "https://<apiId>.execute-api.ap-northeast-2.amazonaws.com/prod/check"
done

# 오늘 Quota 잔여량 확인
aws apigateway get-usage \
  --usage-plan-id <usagePlanId> \
  --key-id <apiKeyId> \
  --start-date $(date -u +%Y-%m-%d) \
  --end-date $(date -u +%Y-%m-%d)
```
