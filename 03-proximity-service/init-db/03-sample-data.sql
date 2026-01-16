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
    'https://www.instagram.com/theclimb_sinsa/',
    '02-549-8821'
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
);
