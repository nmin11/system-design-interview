# Rate Limiter Implementation

다층 방어 구조의 Rate Limiter 시스템입니다. CloudFront/WAF의 IP 기반 제한과 API Gateway의 API Key 기반 제한을 결합했습니다.

## Architecture Diagram

![Rate Limiter Real](../img/rate-limiter-real.svg)

- 본 프로젝트의 실제 구현 아키텍처

### Challengeable

![Rate Limiter Ideal](../img/rate-limiter-ideal.svg)

- 처음에 구현하려던 아키텍처였지만 아래와 같은 문제들로 인해 유기
  - ElastiCache 활용으로 인한 복잡도 증가
  - Grafana 활용을 위한 별도 인프라 구축의 번거로움

## Components

### 1. CloudFront
- **Geo-restriction**: 한국(KR)만 허용
- **Origin**: API Gateway
- **WAF 통합**: WebACL 연결
- **Caching**: 비활성화 (TTL = 0)

### 2. WAF (Web Application Firewall)
- **IP Rate Limiting**: IP당 1000 req/5min
- **CloudWatch Metrics**: 실시간 모니터링

### 3. API Gateway
- **Endpoint**: `/check` (GET)
- **Authentication**: API Key 기반 (`x-api-key` 헤더 필요)
- **Integration**: MOCK (Lambda 없이 정적 응답 반환)

### 4. Usage Plan (Token Bucket Algorithm)
- **Rate Limit**: 100 req/s (초당 평균 요청 수)
- **Burst Limit**: 200 req (버스트 용량, 토큰 버킷 크기)
- **Quota**: 20,000 req/day (일일 요청 할당량)

### 5. Gateway Response
- **THROTTLED**: Rate limit 초과 시 429 응답 + 헤더
- **QUOTA_EXCEEDED**: 일일 할당량 초과 시 429 응답 + 헤더

### 6. API Key
- Usage Plan에 연결된 API Key로 사용자 식별
- 각 API Key별로 독립적인 rate limit 적용

### 7. CloudWatch Alarm
- WAF IP Rate Limit 메트릭 모니터링
- 임계값 초과 시 알람 발생

## Deployment

### Deploy
```bash
cd 01-rate-limiter
pulumi up
```

### Outputs
- `cloudfrontUrl`: CloudFront 배포 URL
- `apiUrl`: API Gateway 직접 접근 URL
- `apiKeyId`: 생성된 API Key의 ID
- `apiKeyValue`: 생성된 API Key의 값 (secret)

### API Key 값 조회

```bash
aws apigateway get-api-key --api-key <apiKeyId> --include-value --query 'value' --output text
```

## Testing

### Jest Testing

통합 테스트는 `test/` 디렉토리에 있으며, 실제 배포된 API Gateway 엔드포인트를 대상으로 실행됩니다.

#### Prerequisites

```bash
cd test

# 의존성 설치
npm install

# 환경변수 설정
cp .env.example .env
# .env 파일을 열어 아래 값들을 실제 배포된 값으로 수정:
# - API_URL: API Gateway URL
# - API_KEY: 발급받은 API Key 값
# - CLOUDFRONT_URL: CloudFront 배포 URL (선택)
# - RATE_LIMIT, BURST_LIMIT, QUOTA_LIMIT: Usage Plan 설정값
```

#### Run Tests

```bash
# 환경 변수 설정 후 테스트
export $(cat .env | xargs) && npm test

# 전체 테스트 실행
npm test

# watch 모드로 실행
npm run test:watch

# 커버리지 포함 실행
npm run test:coverage

# verbose 모드로 실행
npm run test:verbose
```

#### Test Suites

테스트는 다음과 같은 카테고리로 구성되어 있습니다:

1.  **Basic Functionality**: 기본 동작 확인
    - 단일 요청 성공 여부
    - Rate limit 헤더 포함 여부

2.  **Burst Limit Behavior**: 버스트 제한 동작
    - 버스트 한도 내 요청 허용 확인
    - 버스트 한도 초과 요청 거부 확인

3.  **Token Bucket Algorithm**: 토큰 버킷 알고리즘 검증
    - 시간 경과에 따른 토큰 리필 동작 확인

4.  **Error Handling**: 에러 처리
    - 잘못된 API Key 처리

5.  **Performance Characteristics**: 성능 특성
    - 응답 시간 분포 측정

### Manual Testing (curl)

간단한 수동 테스트가 필요한 경우:

```bash
# 정상 요청
API_KEY=$(aws apigateway get-api-key --api-key <apiKeyId> --include-value --query 'value' --output text)
curl -v -H "x-api-key: $API_KEY" https://<cloudfrontUrl>/check

# 응답 예시:
# < HTTP/1.1 200 OK
# < X-RateLimit-Limit: 100
# < X-RateLimit-Burst: 200
# < X-RateLimit-Quota: 10000
# {
#   "message": "Rate limit check passed"
# }
```

## Monitoring

### CloudWatch Metrics

**WAF Metrics** (Namespace: `AWS/WAFV2`)
- `IPRateLimit`: IP별 차단 횟수
- `AllowedRequests`: 허용된 요청 수
- `BlockedRequests`: 차단된 요청 수

**API Gateway Metrics** (Namespace: `AWS/ApiGateway`)
- `Count`: 총 요청 수
- `4XXError`: 클라이언트 오류 (429 포함)
- `Latency`: API 응답 시간

**CloudFront Metrics** (Namespace: `AWS/CloudFront`)
- `Requests`: 총 요청 수
- `BytesDownloaded`: 다운로드된 바이트
- `4xxErrorRate`: 4xx 에러율

### Usage Plan Monitoring

```bash
# 현재 사용량 확인
aws apigateway get-usage \
  --usage-plan-id <usagePlanId> \
  --start-date $(date -u +%Y-%m-%d) \
  --end-date $(date -u +%Y-%m-%d)
```

### WAF Monitoring

```bash
# WAF 차단 로그 확인
aws wafv2 get-sampled-requests \
  --web-acl-arn <wafAclArn> \
  --rule-metric-name IPRateLimit \
  --scope CLOUDFRONT \
  --time-window StartTime=$(date -u -d '1 hour ago' +%s),EndTime=$(date -u +%s) \
  --max-items 100
```

## Response Headers

### 200 OK

| Header | Description | Value |
|--------|-------------|-------|
| `X-RateLimit-Limit` | 초당 최대 요청 수 (steady-state rate) | `100` |
| `X-RateLimit-Burst` | 버스트 용량 (토큰 버킷 크기) | `200` |
| `X-RateLimit-Quota` | 일일 요청 할당량 | `10000` |

### 429 Too Many Requests (Throttled)

| Header | Description | Value |
|--------|-------------|-------|
| `X-RateLimit-Limit` | 초당 최대 요청 수 | `100` |
| `X-RateLimit-Burst` | 버스트 용량 | `200` |

### 429 Too Many Requests (Quota Exceeded)

| Header | Description | Value |
|--------|-------------|-------|
| `X-RateLimit-Quota` | 일일 할당량 | `10000` |

## Cleanup

```bash
pulumi destroy
```
