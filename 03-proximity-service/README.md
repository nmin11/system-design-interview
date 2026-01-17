## 로컬 실행 방법

```sh
cd 03-proximity-service
docker compose up
```

- 이후 `localhost:3000` 으로 접속

## 클라우드 배포 방법

### AWS 배포

```sh
cd infrastructure
pulumi up
```

- 배포 시간 10분 가량 소요

### ECR Push

```sh
cd scripts
./push-to-ecr.sh
```

- 빌드 및 배포 시간 5분 가량 소요

### Bastion EC2에서 DB Init 작업

- AWS EC2 콘솔 혹은 AWS CLI를 통해 배포된 Bastion EC2로 접속
  - Bastion IP 확인 방법: `pulumi stack output bastionPublicIp`

```sh
psql -h proximity-service-db.<instance-id>.ap-northeast-2.rds.amazonaws.com -U proximity -d proximity
```

- DB 비밀번호는 Pulumi config의 dbPassword 값
- 이후 `init-db` 폴더의 쿼리들 하나씩 실행

### Vercel 배포

- GitHub에 `system-design-interview` 레포지토리 생성
- 루트 폴더를 `./03-proximity-service/client`로 설정
- Environment Variables 설정
  - LBS_API_URL: ALB 엔드포인트 입력
  - NEXT_PUBLIC_NAVER_MAP_CLIENT_ID: Naver Map Client ID 입력
- 배포 이후 웹 UI에서 클라이밍 암장 검색 테스트
