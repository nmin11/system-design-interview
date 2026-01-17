#!/bin/bash
set -e

# 색상 정의
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# 프로젝트 루트 디렉토리
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# AWS 설정
AWS_REGION="${AWS_REGION:-ap-northeast-2}"
AWS_ACCOUNT_ID=$(aws sts get-caller-identity --query Account --output text)
ECR_REGISTRY="${AWS_ACCOUNT_ID}.dkr.ecr.${AWS_REGION}.amazonaws.com"

# 이미지 태그 (기본값: latest)
IMAGE_TAG="${1:-latest}"

echo -e "${GREEN}========================================${NC}"
echo -e "${GREEN}ECR Push Script${NC}"
echo -e "${GREEN}========================================${NC}"
echo -e "AWS Account: ${YELLOW}${AWS_ACCOUNT_ID}${NC}"
echo -e "Region: ${YELLOW}${AWS_REGION}${NC}"
echo -e "Image Tag: ${YELLOW}${IMAGE_TAG}${NC}"
echo ""

# ECR 로그인
echo -e "${GREEN}[1/5] ECR 로그인...${NC}"
aws ecr get-login-password --region "$AWS_REGION" | \
  docker login --username AWS --password-stdin "$ECR_REGISTRY"
echo ""

# LBS 빌드
echo -e "${GREEN}[2/5] LBS 빌드...${NC}"
cd "$PROJECT_ROOT/services/lbs"
./gradlew bootJar -q
docker build --platform linux/amd64 -t proximity-service/lbs:${IMAGE_TAG} .
echo ""

# Place Service 빌드
echo -e "${GREEN}[3/5] Place Service 빌드...${NC}"
cd "$PROJECT_ROOT/services/place-service"
./gradlew bootJar -q
docker build --platform linux/amd64 -t proximity-service/place-service:${IMAGE_TAG} .
echo ""

# 태그
echo -e "${GREEN}[4/5] 이미지 태그...${NC}"
docker tag proximity-service/lbs:${IMAGE_TAG} \
  "${ECR_REGISTRY}/proximity-service/lbs:${IMAGE_TAG}"
docker tag proximity-service/place-service:${IMAGE_TAG} \
  "${ECR_REGISTRY}/proximity-service/place-service:${IMAGE_TAG}"
echo ""

# 푸시
echo -e "${GREEN}[5/5] ECR 푸시...${NC}"
docker push "${ECR_REGISTRY}/proximity-service/lbs:${IMAGE_TAG}"
docker push "${ECR_REGISTRY}/proximity-service/place-service:${IMAGE_TAG}"
echo ""

echo -e "${GREEN}========================================${NC}"
echo -e "${GREEN}완료!${NC}"
echo -e "${GREEN}========================================${NC}"
echo -e "LBS: ${YELLOW}${ECR_REGISTRY}/proximity-service/lbs:${IMAGE_TAG}${NC}"
echo -e "Place: ${YELLOW}${ECR_REGISTRY}/proximity-service/place-service:${IMAGE_TAG}${NC}"
