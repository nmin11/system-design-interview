# Nearby Friends 구현 계획

## 아키텍처 개요

```
┌─────────────────────────────────────────────────────────────┐
│                     React Client                             │
│  NaverMap + WebSocket Hook + Geolocation + Friend Markers   │
└──────────────────────────┬──────────────────────────────────┘
                           │ WebSocket (ws://localhost:8080/ws)
                           ▼
┌─────────────────────────────────────────────────────────────┐
│                   Erlang/OTP Application                     │
│                                                              │
│  otp_sup (Top Supervisor, one_for_one)                      │
│      ├── location_registry (gen_server, ETS 기반)           │
│      ├── friend_sup (simple_one_for_one)                    │
│      │       └── 400 x friend_worker (gen_server)           │
│      └── cowboy listener (WebSocket)                        │
└─────────────────────────────────────────────────────────────┘
```

## Erlang 모듈 구조

```
otp/src/
├── otp.app.src              # 애플리케이션 설정 (수정)
├── otp_app.erl              # 애플리케이션 시작점 (기존)
├── otp_sup.erl              # 최상위 슈퍼바이저 (수정)
├── geo_utils.erl            # Haversine, 경계 체크 등
├── korea_boundaries.erl     # 한국/서울 좌표 상수
├── location_registry.erl    # ETS 기반 위치 저장소
├── friend_sup.erl           # 친구 프로세스 슈퍼바이저
├── friend_worker.erl        # 개별 친구 프로세스
└── ws_handler.erl           # Cowboy WebSocket 핸들러
```

## React 컴포넌트 구조

```
client/src/
├── App.tsx                  # 메인 컴포넌트 (수정)
├── components/
│   ├── NaverMap.tsx         # 네이버 지도 래퍼
│   ├── FriendMarkers.tsx    # 친구 마커들
│   └── StatusPanel.tsx      # 연결 상태, 친구 수
├── hooks/
│   ├── useWebSocket.ts      # WebSocket 연결 관리
│   └── useGeolocation.ts    # 디바이스 위치
└── types/
    ├── friend.ts            # Friend 타입
    └── naver-maps.d.ts      # Naver Maps 타입 선언
```

## WebSocket 메시지 프로토콜

```json
// Client -> Server
{"type": "search", "lat": 37.5665, "lng": 126.978}

// Server -> Client
{"type": "friends", "data": [
  {"id": "friend_001", "lat": 37.57, "lng": 126.98, "distance": 0.42}
]}
```

## 구현 단계

### Phase 1: Erlang 기반 인프라

1. `rebar.config` 의존성 추가 (cowboy, jsx)
2. `geo_utils.erl` - Haversine 거리 계산, 좌표 이동
3. `korea_boundaries.erl` - 한국/서울 경계 상수
4. `location_registry.erl` - ETS 기반 위치 저장 및 검색

### Phase 2: 친구 프로세스 시스템

1. `friend_worker.erl` - 개별 친구 gen_server
2. `friend_sup.erl` - simple_one_for_one 슈퍼바이저
3. `otp_sup.erl` 수정 - 자식 프로세스 등록

### Phase 3: WebSocket 레이어

1. `ws_handler.erl` - Cowboy WebSocket 핸들러
2. `otp_sup.erl`에 Cowboy 리스너 추가
3. 15초 주기 업데이트 타이머

### Phase 4: React 프론트엔드

1. `index.html`에 Naver Map 스크립트 추가
2. 타입 정의 파일들 생성
3. `useWebSocket.ts`, `useGeolocation.ts` 훅
4. `NaverMap.tsx` 컴포넌트
5. 친구 마커 및 8km 반경 원 표시

## 핵심 알고리즘

- **Haversine**: 두 좌표 간 거리 계산 (km)
- **Move Point**: 방위각과 거리로 새 좌표 계산
- **Point in Polygon**: Ray casting으로 한국 내 위치 확인
- **Grid Indexing**: ~1km 그리드 셀로 빠른 근처 친구 검색

## 수정할 파일

- `otp/rebar.config` - cowboy, jsx 의존성
- `otp/src/otp.app.src` - 애플리케이션 설정
- `otp/src/otp_sup.erl` - 슈퍼비전 트리
- `client/index.html` - Naver Map 스크립트
- `client/src/App.tsx` - 메인 컴포넌트

## 검증 방법

1. Erlang 서버 시작: `rebar3 shell`
2. React 클라이언트 시작: `npm run dev`
3. 브라우저에서 지도 확인 및 친구 마커 표시 확인
4. 15초마다 친구 위치 갱신 확인
5. 지도 클릭으로 검색 위치 변경 테스트
