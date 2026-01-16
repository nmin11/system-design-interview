## 인프라 구성

### ALB (Application Load Balancer)

- Target Group 1: LBS
- Target Group 2: 사업장 서비스

### Fargate

- LBS 태스크
- 사업장 서비스 태스크

### RDS

- PostgreSQL + PostGIS
- Read Replica

### ElastiCache (Optional)

- Redis 활용
- Geohash 기반 캐싱

### VPC

sketch

```
VPC (10.0.0.0/16)
├─ Public Subnet (10.0.1.0/24, 10.0.2.0/24) - ALB
├─ Private Subnet (10.0.10.0/24, 10.0.11.0/24) - Fargate
└─ Private Subnet (10.0.20.0/24, 10.0.21.0/24) - RDS, Redis
```
