# Nearby Friends

System Design Interview 2권 2장 "Nearby Friends"의 Redis Pub/Sub 부분을 Erlang/OTP로 대체 구현한 프로젝트입니다.

## Redis Pub/Sub vs Erlang/OTP 비교

### 원본 설계 (책) vs 본 구현

System Design Interview 2권에서 제안하는 아키텍처는 WebSocket 서버 + Redis Pub/Sub 조합입니다.
본 프로젝트는 Redis Pub/Sub 부분을 Erlang/OTP의 프로세스 모델로 대체하여 구현했습니다.

### 아키텍처 비교

| 구분 | Redis Pub/Sub 기반 | Erlang/OTP 기반 (본 구현) |
|------|-------------------|-------------------------|
| **메시지 브로커** | Redis (외부 인프라) | Erlang 프로세스 (내장) |
| **구독/발행** | Redis Channel 구독 | 프로세스 간 직접 메시지 전달 |
| **상태 저장** | Redis Hash/Sorted Set | ETS 테이블 |
| **연결 관리** | WebSocket 서버 + Redis 연결 | Cowboy WebSocket 핸들러 |
| **장애 복구** | Redis Sentinel/Cluster | OTP Supervisor Tree |

### 상세 비교표

| 항목 | Redis Pub/Sub | Erlang/OTP | 비고 |
|------|--------------|------------|------|
| **지연 시간** | 네트워크 홉 추가 (App ↔ Redis) | 프로세스 내 직접 통신 | Erlang이 더 낮은 지연 |
| **운영 복잡도** | Redis 클러스터 운영 필요 | 단일 Erlang 노드로 충분 | 인프라 단순화 |
| **수평 확장** | Redis 클러스터 + Consistent Hashing | Erlang Distribution + pg 모듈 | 둘 다 확장 가능 |
| **메모리 효율** | Redis 메모리 + App 메모리 | 단일 VM 내 공유 | Erlang이 더 효율적 |
| **장애 격리** | Redis 장애 시 전체 영향 | 개별 프로세스 격리 | Erlang의 "Let it crash" |
| **코드 복잡도** | Pub/Sub 라이브러리 의존 | gen_server 패턴 | 비슷한 수준 |

### Erlang/OTP 선택의 특장점

#### 1. 인프라 단순화

```
[Redis 기반]                         [Erlang 기반]
┌─────────────┐                      ┌─────────────┐
│ WebSocket   │                      │   Erlang    │
│   Server    │◄──────┐              │ Application │
└─────────────┘       │              │             │
       │              │              │ - WebSocket │
       ▼              │              │ - Pub/Sub   │
┌─────────────┐       │              │ - Storage   │
│   Redis     │───────┘              └─────────────┘
│  Pub/Sub    │                           ▲
└─────────────┘                      단일 프로세스로 통합
       │
운영해야 할 외부 인프라
```

#### 2. Actor Model의 자연스러운 매핑

| 도메인 개념 | Redis 기반 | Erlang 기반 |
|------------|-----------|-------------|
| 사용자 | Redis Key | **프로세스** (friend_worker) |
| 위치 정보 | Redis Hash | **프로세스 상태** + ETS |
| 구독 관계 | Redis Channel | **프로세스 간 링크/모니터** |
| 브로드캐스트 | PUBLISH 명령 | **메시지 전송** (!) |

각 친구가 독립적인 프로세스로 존재하여 실제 도메인 모델과 1:1 매핑됩니다.

#### 3. 장애 복구 (Fault Tolerance)

```
[Redis 장애 시]
Redis 다운 → 모든 Pub/Sub 중단 → 전체 서비스 장애

[Erlang 장애 시]
friend_worker 크래시 → 해당 프로세스만 재시작 → 다른 친구들 영향 없음
```

#### 4. 성능 특성

| 메트릭 | Redis Pub/Sub | Erlang/OTP |
|--------|--------------|------------|
| 메시지 지연 | ~1-5ms (네트워크) | ~μs (프로세스 내) |
| 동시 연결 | Redis 연결 풀 한계 | 수백만 프로세스 가능 |
| GC 영향 | 전체 앱 Stop-the-world | 프로세스별 개별 GC |
| 메모리 | 직렬화 오버헤드 | 바이너리 공유 |

### 트레이드오프

| 장점 | 단점 |
|------|------|
| ✅ 외부 인프라 의존성 제거 | ❌ Erlang 학습 곡선 |
| ✅ 낮은 지연 시간 | ❌ 생태계가 작음 |
| ✅ 강력한 장애 격리 | ❌ 디버깅 도구 제한적 |
| ✅ 도메인 모델과 자연스러운 매핑 | ❌ 팀 내 Erlang 경험 필요 |
| ✅ 단일 배포 단위 | ❌ 다른 서비스와 통합 시 복잡 |

### 언제 어떤 것을 선택할까?

| 상황 | 추천 |
|------|------|
| 이미 Redis 인프라가 있음 | Redis Pub/Sub |
| 실시간 요구사항이 엄격함 | **Erlang/OTP** |
| 팀이 Erlang에 익숙함 | **Erlang/OTP** |
| 마이크로서비스 아키텍처 | Redis Pub/Sub |
| 단일 서비스로 완결 | **Erlang/OTP** |
| 수백만 동시 연결 필요 | **Erlang/OTP** |

## 아키텍처 개요

```
┌─────────────────────────────────────────────────────────────┐
│                     React Client                             │
│         (Naver Map + WebSocket + Geolocation)               │
└──────────────────────────┬──────────────────────────────────┘
                           │ WebSocket (ws://localhost:8080/ws)
                           ▼
┌─────────────────────────────────────────────────────────────┐
│                   Erlang/OTP Application                     │
│                                                              │
│  ┌─────────────────────────────────────────────────────┐    │
│  │              otp_sup (Top Supervisor)                │    │
│  │                 strategy: one_for_one                │    │
│  │                intensity: 5, period: 10              │    │
│  └────────┬─────────────────────────┬───────────────────┘    │
│           │                         │                        │
│           ▼                         ▼                        │
│  ┌─────────────────┐      ┌─────────────────────────┐       │
│  │location_registry│      │      friend_sup         │       │
│  │  (gen_server)   │      │  strategy: simple_one_  │       │
│  │                 │      │          for_one        │       │
│  │  - ETS 기반     │      │  intensity: 10          │       │
│  │  - 위치 저장    │      │  period: 60             │       │
│  │  - 근처 검색    │      └───────────┬─────────────┘       │
│  └─────────────────┘                  │                      │
│                                       ▼                      │
│                         ┌─────────────────────────┐          │
│                         │  400 x friend_worker    │          │
│                         │     (gen_server)        │          │
│                         │                         │          │
│                         │  - 랜덤 이동 (4-60km/h) │          │
│                         │  - 1초마다 위치 갱신    │          │
│                         │  - 한국 경계 내 이동    │          │
│                         └─────────────────────────┘          │
│                                                              │
│  ┌─────────────────────────────────────────────────────┐    │
│  │           Cowboy WebSocket Server (:8080)            │    │
│  │                                                      │    │
│  │  ws_handler: 클라이언트 연결 관리                    │    │
│  │  - 검색 요청 처리                                    │    │
│  │  - 15초 주기 위치 업데이트 푸시                      │    │
│  └─────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────┘
```

## Supervision Tree 구조

### 계층 구조

```
otp_sup (one_for_one, permanent)
    │
    ├── location_registry (worker, permanent)
    │       └── ETS 테이블: friend_locations
    │
    └── friend_sup (supervisor, permanent)
            │
            └── [simple_one_for_one]
                    ├── friend_worker_0001 (worker, transient)
                    ├── friend_worker_0002 (worker, transient)
                    ├── ...
                    └── friend_worker_0400 (worker, transient)
```

### Supervisor 전략 설명

| Supervisor | 전략 | 설명 |
|------------|------|------|
| `otp_sup` | `one_for_one` | 자식 프로세스가 죽으면 해당 프로세스만 재시작 |
| `friend_sup` | `simple_one_for_one` | 동적으로 동일한 타입의 자식 프로세스를 생성 |

### 재시작 정책

| 프로세스 | restart | 설명 |
|----------|---------|------|
| `location_registry` | `permanent` | 항상 재시작 (핵심 서비스) |
| `friend_sup` | `permanent` | 항상 재시작 (친구 프로세스 관리자) |
| `friend_worker` | `transient` | 비정상 종료 시에만 재시작 |

## 실패 처리 (Fault Tolerance)

### 1. location_registry 실패

```
[상황] location_registry 크래시 발생
    │
    ▼
[otp_sup] one_for_one 전략에 따라 location_registry만 재시작
    │
    ▼
[location_registry] 새 ETS 테이블 생성
    │
    ▼
[friend_worker들] 다음 위치 업데이트 시 자동으로 ETS에 재등록
```

- `intensity: 5, period: 10` - 10초 내 5번 이상 크래시 시 전체 supervisor 종료
- ETS 테이블이 재생성되면 friend_worker들이 1초 주기로 위치를 업데이트하므로 자동 복구

### 2. friend_worker 실패

```
[상황] friend_worker 개별 크래시 발생
    │
    ▼
[friend_sup] simple_one_for_one 전략에 따라 해당 worker만 재시작
    │
    ▼
[friend_worker] init/1에서 새로운 랜덤 위치로 초기화
    │
    ▼
[location_registry] 새 위치로 자동 등록
```

- `restart: transient` - 비정상 종료 시에만 재시작
- `intensity: 10, period: 60` - 60초 내 10번 이상 크래시 시 friend_sup 종료

### 3. friend_sup 실패

```
[상황] friend_sup 크래시 발생
    │
    ▼
[otp_sup] one_for_one 전략에 따라 friend_sup만 재시작
    │
    ▼
[friend_sup] 재시작, 하지만 자식 프로세스(friend_worker)는 없음
    │
    ▼
[수동 복구 필요] otp_sup:spawn_friends() 호출 필요
```

### 4. Cowboy WebSocket 연결 실패

```
[상황] WebSocket 연결 끊김
    │
    ▼
[ws_handler] terminate/3 콜백에서 타이머 정리
    │
    ▼
[React Client] 자동 재연결 (3초 후)
    │
    ▼
[ws_handler] 새 연결에서 검색 재요청
```

## 주요 모듈 설명

### Erlang (otp/)

| 모듈 | 역할 |
|------|------|
| `otp_app` | OTP 애플리케이션 시작점 |
| `otp_sup` | 최상위 슈퍼바이저, Cowboy 서버 시작 |
| `location_registry` | ETS 기반 위치 저장소, 8km 반경 검색 |
| `friend_sup` | 친구 프로세스 동적 슈퍼바이저 |
| `friend_worker` | 개별 친구 프로세스, 랜덤 이동 |
| `ws_handler` | WebSocket 핸들러, 15초 주기 업데이트 |
| `geo_utils` | Haversine 거리 계산, 좌표 이동 |
| `korea_boundaries` | 한국/서울 경계 좌표, 랜덤 위치 생성 |

### React (client/)

| 파일 | 역할 |
|------|------|
| `App.tsx` | 메인 컴포넌트 |
| `components/NaverMap.tsx` | 네이버 지도, 마커, 반경 원 |
| `components/StatusPanel.tsx` | 연결 상태, 친구 수 표시 |
| `hooks/useWebSocket.ts` | WebSocket 연결 관리 |
| `hooks/useGeolocation.ts` | 디바이스 위치 |

## 실행 방법

### 1. 환경 설정

```bash
# client/.env 파일 생성
cp client/.env.example client/.env
# VITE_NAVER_CLIENT_ID에 실제 Naver Cloud Platform Client ID 입력
```

### 2. Erlang 서버 실행

```bash
cd otp
rebar3 shell
```

출력 예시:
```
WebSocket server started on port 8080
Spawning 400 friends (100 in Seoul, 300 elsewhere in Korea)...
All friends spawned successfully!
```

### 3. React 클라이언트 실행

```bash
cd client
npm install
npm run dev
```

브라우저에서 `http://localhost:5173` 접속

## 기능

- 400명의 가상 친구가 한국 전역에서 랜덤하게 이동 (100명은 서울)
- 8km 반경 내 친구 검색
- 15초마다 자동 위치 갱신
- 지도 클릭으로 검색 위치 변경
- 디바이스 GPS 위치 사용 가능

## 기술 스택

- **Backend**: Erlang/OTP, Cowboy (WebSocket), jsx (JSON)
- **Frontend**: React 19, TypeScript, Vite, Naver Maps API
