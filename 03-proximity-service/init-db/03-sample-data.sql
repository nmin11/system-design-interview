-- 샘플 클라이밍 암장 데이터
INSERT INTO places (name, image_url, street_address, lot_number, location, description, instagram_url, phone_number) VALUES
(
    '클라이밍파크 강남점',
    'https://search.pstatic.net/common/?src=https%3A%2F%2Fldb-phinf.pstatic.net%2F20241207_296%2F1733574916131VdLun_JPEG%2FIMG_20241112_144412_746.jpg',
    '서울 강남구 강남대로 364 지하1층',
    '서울 강남구 역삼동 826-21',
    ST_SetSRID(ST_MakePoint(127.029342, 37.495547), 4326),
    E'강남역 4번 출구 바로 앞에 있는 미왕빌딩 지하 1층에 있습니다.\n건물 안으로 들어올 필요 없이 건물 외부에 있는 엘리베이터를 이용해서 지하 1층으로 내려오면 됩니다.',
    'https://www.instagram.com/climbing_park_gangnam',
    '0507-1391-4662'
),
(
    '클라이밍파크 신논현점',
    'https://search.pstatic.net/common/?src=https%3A%2F%2Fldb-phinf.pstatic.net%2F20241207_78%2F1733574894879Ya4yG_JPEG%2FIMG_20241127_230612_020.jpg',
    '서울 강남구 강남대로 468 지하3층',
    '서울 강남구 역삼동 808-4',
    ST_SetSRID(ST_MakePoint(127.025105, 37.504164), 4326),
    E'강남역 4번 출구 바로 앞에 있는 미왕빌딩 지하 1층에 있습니다.\n건물 안으로 들어올 필요 없이 건물 외부에 있는 엘리베이터를 이용해서 지하 1층으로 내려오면 됩니다.',
    'https://www.instagram.com/climbing_park_gangnam',
    '0507-1362-4662'
),
(
    '클라이밍파크 성수점',
    'https://search.pstatic.net/common/?src=https%3A%2F%2Fldb-phinf.pstatic.net%2F20230703_93%2F168835986963333Rx7_JPEG%2FIMG_8010.jpeg',
    '서울 성동구 연무장13길 7 매니아빌딩',
    '서울 성동구 성수동2가 273-34',
    ST_SetSRID(ST_MakePoint(127.058085, 37.542305), 4326),
    '성수역 3번 출구에서 4분 거리에 위치',
    'https://www.instagram.com/climbing_park_seongsu',
    '0507-1322-4662'
),
(
    '더클라임 연남점',
    'https://search.pstatic.net/common/?src=https%3A%2F%2Fnaverbooking-phinf.pstatic.net%2F20230726_2%2F1690338434284FkXEj_JPEG%2F20230707_GirlsonTop-143.jpg',
    '서울 마포구 양화로 186 3층',
    '서울 마포구 동교동 167-2',
    ST_SetSRID(ST_MakePoint(126.925796, 37.557664), 4326),
    '신논현역 5번 출구에서 다섯 걸음',
    'https://www.instagram.com/climbing_park',
    '02-2088-5071'
),
(
    '더클라임 강남점',
    'https://search.pstatic.net/common/?src=https%3A%2F%2Fldb-phinf.pstatic.net%2F20221025_87%2F1666674763427DfSNN_JPEG%2FCCA27AB7-09DA-40C9-862F-B3EF2FC8441D.jpeg',
    '서울 강남구 테헤란로8길 21 화인강남빌딩 B1층',
    '서울 강남구 역삼동 823-14',
    ST_SetSRID(ST_MakePoint(127.031981, 37.497515), 4326),
    E'강남역 1, 2, 3번 출구 5분 거리에 위치해 있습니다.\n주차: 건물 내 지하 주차장 가능 30분 할인권 적용 가능 (이후 10분당 1,000원)',
    'https://www.instagram.com/theclimb_gangnam',
    '02-566-8821'
),
(
    '더클라임 신사점',
    'https://search.pstatic.net/common/?src=https%3A%2F%2Fldb-phinf.pstatic.net%2F20230831_238%2F1693458688699vBrUq_JPEG%2FKakaoTalk_20230831_140613286_02.jpg',
    '서울 강남구 압구정로2길 6 지하2층',
    '서울 강남구 신사동 505-7',
    ST_SetSRID(ST_MakePoint(127.019136, 37.521095), 4326),
    '지하철 신사역 3호선 6번 출구 도보 9분 거리',
    'https://www.instagram.com/theclimb_sinsa',
    '02-549-8821'
),
(
    '더클라임 양재점',
    'https://search.pstatic.net/common/?src=https%3A%2F%2Fldb-phinf.pstatic.net%2F20210120_166%2F1611108698158uK8M4_PNG%2F%25BE%25E7%25C0%25E7%25C1%25A1%25C7%25C1%25B7%25CE%25BF%25EC.png',
    '서울 강남구 남부순환로 2615 지하1층',
    '서울 강남구 도곡동 957-11',
    ST_SetSRID(ST_MakePoint(127.035859, 37.485138), 4326),
    '3호선 양재역 4번 출구에서 직진 (도보 2분 거리)',
    'https://www.instagram.com/theclimb_yangjae',
    '02-576-8821'
),
(
    '더클라임 논현점',
    'https://search.pstatic.net/common/?src=https%3A%2F%2Fldb-phinf.pstatic.net%2F20231206_215%2F1701834130082zQhOO_JPEG%2FKakaoTalk_20231206_123656353_03.jpg',
    '서울 서초구 강남대로 519 지하1층',
    '서울 서초구 반포동 723-1',
    ST_SetSRID(ST_MakePoint(127.022262, 37.508291), 4326),
    '논현역 4번 출구 2분 거리 및 신논현역 2번 출구 6분 거리',
    'https://www.instagram.com/theclimb_nonhyeon',
    '02-545-5014'
),
(
    '더클라임 성수점',
    'https://search.pstatic.net/common/?src=https%3A%2F%2Fldb-phinf.pstatic.net%2F20250220_72%2F1740023651011uJksd_JPEG%2FKakaoTalk_20250220_124855630_19.jpg',
    '서울 성동구 아차산로17길 49 생각공장 데시앙플렉스 B1층',
    '서울 성동구 성수동2가 280-6',
    ST_SetSRID(ST_MakePoint(127.065049, 37.546387), 4326),
    E'2호선 성수역, 건대입구역 2번 출구 도보 15분\n7호선 어린이대공원 4번 출구 도보 13분',
    'https://www.instagram.com/theclimb_seongsu',
    '02-499-5014'
),
(
    '서울숲클라이밍 잠실점',
    'https://search.pstatic.net/common/?src=https%3A%2F%2Fldb-phinf.pstatic.net%2F20230714_100%2F1689344535905hk4jh_PNG%2F%25C0%25E1%25BD%25C7%25BD%25A3_%25B7%25CE%25B0%25ED.png',
    '서울 송파구 백제고분로7길 49 지하1층',
    '서울 송파구 잠실동 183-4',
    ST_SetSRID(ST_MakePoint(127.084402, 37.510832), 4326),
    '잠실새내역 4번 출구 버스정류장 도보 2분',
    'https://www.instagram.com/seoulforest_jamsil',
    '010-7710-2703'
),
(
    '서울숲클라이밍 종로점',
    'https://search.pstatic.net/common/?src=https%3A%2F%2Fldb-phinf.pstatic.net%2F20240127_217%2F1706323086285X8NHz_PNG%2F%25C1%25BE%25B7%25CE%25BD%25A3%25B7%25CE%25B0%25ED_%25C8%25F2%25BB%25F6%25B9%25E8%25B0%25E6.png',
    '서울 종로구 수표로 96 지하1층',
    '서울 종로구 관수동 20',
    ST_SetSRID(ST_MakePoint(126.989978, 37.569742), 4326),
    E'종로3가역 15번 출구에서 도보 3분\n* 건물 입구를 바라보고 오른편에 서울숲클라이밍으로 연결되는 계단 입구가 따로 있습니다.\n* 엘리베이터 이용 시, 4호기 엘리베이터만 이용 가능합니다.',
    'https://www.instagram.com/seoulforest_jongro',
    '010-3289-2705'
),
(
    '서울숲클라이밍 영등포점',
    'https://search.pstatic.net/common/?src=https%3A%2F%2Fldb-phinf.pstatic.net%2F20230714_280%2F1689343619976LmDsS_PNG%2F%25BF%25B5%25B5%25EE%25BD%25A3_%25B7%25CE%25B0%25ED.png',
    '서울 영등포구 문래로 164 1층',
    '서울 영등포구 문래동3가 55-16',
    ST_SetSRID(ST_MakePoint(126.899602, 37.517355), 4326),
    E'문래역 5번 출구로 나와 400m 직진 SK리더스뷰 1층 서울숲클라이밍\n규수당 카페 맞은편 건물 1층에 위치해 있습니다.',
    'https://www.instagram.com/seoulforest_yeongdp',
    '010-6686-2700'
),
(
    '손상원 클라이밍짐 을지로점',
    'https://search.pstatic.net/common/?src=https%3A%2F%2Fnaverbooking-phinf.pstatic.net%2F20250224_192%2F1740385369910gtBYj_PNG%2F%25B3%25D7%25C0%25CC%25B9%25F6-%25C7%25C3%25B7%25B9%25C0%25CC%25BD%25BA-%25BB%25E7%25C1%25F8%25B5%25E9-002.png',
    '서울 중구 남대문로 125 지하1층 손상원 클라이밍짐 을지로점',
    '서울 중구 다동 85',
    ST_SetSRID(ST_MakePoint(126.982328, 37.568376), 4326),
    E'을지로입구역 2번 출구 226m 도보 3분\n종각역 5번 출구에서 158m 도보 2분',
    'https://www.instagram.com/sonclimb_euljiro',
    '02-318-3094'
),
(
    '손상원 클라이밍짐 강남역점',
    'https://search.pstatic.net/common/?src=https%3A%2F%2Fldb-phinf.pstatic.net%2F20220814_180%2F1660464969866l39iJ_JPEG%2FKakaoTalk_20220814_170616533_07.jpg',
    '서울 서초구 강남대로 331 지하1층',
    '서울 서초구 서초동 1337-32',
    ST_SetSRID(ST_MakePoint(127.029574, 37.492581), 4326),
    '강남역 5번 출구에서 직진, 도보 4분 소요',
    'https://www.instagram.com/sonclimb_gangnam',
    '02-523-3094'
),
(
    '손상원 클라이밍짐 판교점',
    'https://search.pstatic.net/common/?src=https%3A%2F%2Fldb-phinf.pstatic.net%2F20250224_164%2F1740398898241cEcKX_JPEG%2FKakaoTalk_20240509_151136463.jpg',
    '경기 성남시 분당구 대왕판교로 670',
    '경기 성남시 분당구 삼평동 682',
    ST_SetSRID(ST_MakePoint(127.106997, 37.402075), 4326),
    '유스페이스2 B동 지하 1층에 위치해 있습니다.',
    'https://www.instagram.com/sonclimb_pangyo',
    '031-739-8332'
),
(
    'PEAKERS 클라이밍 종로',
    'https://search.pstatic.net/common/?src=https%3A%2F%2Fldb-phinf.pstatic.net%2F20220107_161%2F1641527724261UnAmA_JPEG%2F1641527716061.jpg',
    '서울 종로구 돈화문로5가길 1 지하 4층',
    '서울 종로구 돈의동 137',
    ST_SetSRID(ST_MakePoint(126.999809, 37.571411), 4326),
    '종로3가역 2-1번 출구에서 지하 통로를 통해 CGV로 들어오신 후 에스컬레이터를 타고 내려오세요.',
    'https://www.instagram.com/peakers_jongro',
    '02-526-8862'
),
(
    'PEAKERS 클라이밍 신촌',
    'https://search.pstatic.net/common/?src=https%3A%2F%2Fldb-phinf.pstatic.net%2F20240202_209%2F1706856154308HP6BN_JPEG%2F%25C7%25C7%25C4%25BF%25BD%25BA_%25BD%25C5%25C3%25CC-5.jpg',
    '서울 서대문구 신촌로 129 아트레온 11층',
    '서울 서대문구 창천동 20-25',
    ST_SetSRID(ST_MakePoint(126.940234, 37.556561), 4326),
    NULL,
    'https://www.instagram.com/peakers_sinchon',
    '02-526-8989'
),
(
    'PEAKERS 클라이밍 구로',
    'https://search.pstatic.net/common/?src=https%3A%2F%2Fldb-phinf.pstatic.net%2F20221104_212%2F1667547245961OFnPJ_JPEG%2FDSC02249.jpg',
    '서울 구로구 구로중앙로 152 NC신구로점 6층',
    '서울 구로구 구로동 573',
    ST_SetSRID(ST_MakePoint(126.882796, 37.501206), 4326),
    '구로역 1번 출구 통로를 건너 NC 신구로점 입장 후 엘리베이터, 에스컬레이터 탑승 후 6층',
    'https://www.instagram.com/peakers_guro',
    '02-526-8850'
),
(
    '그립픽클라이밍짐',
    'https://search.pstatic.net/common/?src=https%3A%2F%2Fldb-phinf.pstatic.net%2F20221213_203%2F167091611785778k6K_JPEG%2FKakaoTalk_20221213_161750428_01.jpg',
    '경기 성남시 수정구 위례광장로 104 지하1층 119~122,127~130호',
    '경기 성남시 수정구 창곡동 505',
    ST_SetSRID(ST_MakePoint(127.141259, 37.473044), 4326),
    '위례광장로 한화 오벨리스크 건물 지하 1층으로 오시면 롯데시네마를 지나 에스컬레이터 쪽에 그립픽 클라이밍장이 바로 보입니다.',
    'https://www.instagram.com/grippick_climbing_wiryecenter',
    '0507-1460-0343'
),
(
    '에어즈락 클라이밍 위례점',
    'https://search.pstatic.net/common/?src=https%3A%2F%2Fldb-phinf.pstatic.net%2F20251209_249%2F1765284235198G8Svk_JPEG%2FKakaoTalk_20200731_223450275_22.jpg',
    '서울 송파구 위례광장로 188 아이온스퀘어 12층',
    '서울 송파구 장지동 881',
    ST_SetSRID(ST_MakePoint(127.142443, 37.481154), 4326),
    NULL,
    'https://www.instagram.com/ayersrock_wirye',
    '02-400-3845'
),
(
    '에어즈락 클라이밍 범계점',
    'https://search.pstatic.net/common/?src=https%3A%2F%2Fnaverbooking-phinf.pstatic.net%2F20230917_106%2F1694936265820hMlD1_JPEG%2FIMG_20230917_101457.JPG',
    '경기 안양시 동안구 시민대로 161 201호',
    '경기 안양시 동안구 비산동 1107',
    ST_SetSRID(ST_MakePoint(126.949168, 37.390485), 4326),
    '범계역 8번 출구에서 바로 앞 횡단보도를 건너면 보이는 건물 2층입니다. (도보 1분 이내)',
    'https://www.instagram.com/ayersrock_beomgye',
    '031-381-3845'
),
(
    '클라임투더문',
    'https://search.pstatic.net/common/?src=https%3A%2F%2Fnaverbooking-phinf.pstatic.net%2F20240301_139%2F17092805870067I7FY_JPEG%2F%25B7%25CE%25B0%25ED.jpg',
    '서울 송파구 백제고분로 224 창대빌딩 지하1층',
    '서울 송파구 삼전동 49-1',
    ST_SetSRID(ST_MakePoint(127.091314, 37.503153), 4326),
    NULL,
    'https://www.instagram.com/climb_t_t_moon',
    '0507-1431-5019'
),
(
    '신촌담장',
    'https://search.pstatic.net/common/?src=https%3A%2F%2Fldb-phinf.pstatic.net%2F20250602_104%2F1748848021367iCpIW_PNG%2F%25C1%25A6%25B8%25F1%25C0%25BB_%25C0%25D4%25B7%25C2%25C7%25D8%25C1%25D6%25BC%25BC%25BF%25E4_-002_%25281%2529.png',
    '서울 서대문구 신촌역로 10 5층',
    '서울 서대문구 대현동 101-7',
    ST_SetSRID(ST_MakePoint(126.943169, 37.557541), 4326),
    NULL,
    'https://www.instagram.com/sinchon_damjang',
    '02-313-1003'
),
(
    '훅클라이밍 왕십리점',
    'https://search.pstatic.net/common/?src=https%3A%2F%2Fnaverbooking-phinf.pstatic.net%2F20251207_112%2F17650960264906xyNg_JPEG%2FHookClimbingW_3-1.jpg',
    '서울 성동구 고산자로6길 40 2층 훅클라이밍',
    '서울 성동구 행당동 140',
    ST_SetSRID(ST_MakePoint(127.035556, 37.558262), 4326),
    '왕십리역 9번 출구 레몬프라자 2층',
    'https://www.instagram.com/hook_wangsimni',
    '02-2282-4739'
),
(
    '훅클라이밍 성수점',
    'https://search.pstatic.net/common/?src=https%3A%2F%2Fnaverbooking-phinf.pstatic.net%2F20241106_164%2F17308608015835G92O_JPEG%2FIMG_5584.jpeg',
    '서울 성동구 성수일로12길 34 3층',
    '서울 성동구 성수동2가 289-211',
    ST_SetSRID(ST_MakePoint(127.054570, 37.547495), 4326),
    '성수역 1번 출구',
    'https://www.instagram.com/hook_seongsu',
    '02-6082-4739'
);
