![Rate Limiter](../img/rate-limiter.svg)

# Rate Limiter Implementation

다층 방어 구조의 Rate Limiter 시스템입니다.

## Architecture

```
Client → CloudFront (Geo-restriction) → WAF (IP Rate Limit) → API Gateway → Lambda Authorizer (Sliding Window) → Backend Lambda
                                                                                        ↓
                                                                                    Redis Cluster
```

## Components

### 1. CloudFront
- **Geo-restriction**: 한국, 미국, 일본만 허용
- **Origin**: API Gateway
- **WAF 통합**: WebACL 연결

### 2. WAF (Web Application Firewall)
- **IP Rate Limiting**: IP당 2000 req/5min
- **Geo Blocking**: 특정 국가 차단 (KP, IR, SY)
- **CloudWatch Metrics**: 실시간 모니터링

### 3. API Gateway
- **Custom Authorizer**: Lambda Authorizer 사용
- **Authorization TTL**: 0 (캐싱 비활성화)
- **Endpoint**: `/check`

### 4. Lambda Authorizer
- **Algorithm**: Sliding Window Counter
- **Window Size**: 60초
- **Max Requests**: 100 req/min
- **Storage**: Redis Sorted Set
- **Fail-open**: Redis 장애 시 요청 허용

### 5. Redis Cluster
- **Engine**: Redis 7.0
- **Deployment**: Multi-AZ (2 nodes)
- **Encryption**: At-rest 암호화 활성화
- **Backup**: 일일 스냅샷

### 6. CloudWatch
- **Metrics**: WAF 룰별 메트릭 수집
- **Alarms**: IP Rate Limit 임계값 초과 시 알람

## Deployment

### Prerequisites
```bash
# Lambda Layer용 의존성 설치
cd lambda/layer
npm install
cd ../..
```

### Deploy
```bash
pulumi up
```

### Outputs
- `cloudfrontUrl`: CloudFront 배포 URL
- `apiGatewayUrl`: API Gateway URL (direct access)
- `redisEndpoint`: Redis 클러스터 엔드포인트

## Testing

### 정상 요청
```bash
curl https://<cloudfrontUrl>/check
```

### Rate Limit 테스트
```bash
# 100번 연속 요청으로 Sliding Window 테스트
for i in {1..100}; do
  curl -w "\n%{http_code}\n" https://<cloudfrontUrl>/check
  sleep 0.5
done
```

### Geo Blocking 테스트
```bash
# VPN으로 차단된 국가에서 접속 시도
curl https://<cloudfrontUrl>/check
# Expected: 403 Forbidden
```

## Monitoring

### CloudWatch Dashboards
- WAF 메트릭: `AWS/WAFV2`
- Lambda 메트릭: `AWS/Lambda`
- API Gateway 메트릭: `AWS/ApiGateway`

### Key Metrics
- `IPRateLimit`: IP별 차단 횟수
- `GeoBlocking`: Geo 차단 횟수
- `AuthorizerLatency`: Lambda Authorizer 응답 시간
- `RedisConnectionErrors`: Redis 연결 오류

## Sliding Window Counter Algorithm

```
Time:     [-------- 60s window --------]
Requests: |--x--x----x-x--x--x-x-x-x---|

1. 현재 시간 - 60초 범위 밖 요청 제거 (zRemRangeByScore)
2. 윈도우 내 요청 수 카운트 (zCard)
3. 임계값 체크 (100 req/min)
4. 통과 시 현재 요청 추가 (zAdd)
```

## Cost Optimization

- NAT Gateway: Single (Multi-AZ 대신)
- Redis: t3.micro (프로덕션에서는 더 큰 인스턴스 권장)
- Lambda: VPC 내 배치로 Redis 직접 연결

## Cleanup

```bash
pulumi destroy
```
