-- Places 테이블 생성
CREATE TABLE IF NOT EXISTS places (
    id BIGSERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    image_url VARCHAR(500) NOT NULL,
    street_address VARCHAR(500) NOT NULL,
    lot_number VARCHAR(100) NOT NULL,
    location geometry(Point, 4326) NOT NULL,
    description TEXT,
    instagram_url VARCHAR(500),
    phone_number VARCHAR(50),
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- 공간 인덱스 생성
CREATE INDEX IF NOT EXISTS idx_places_location ON places USING GIST (location);

-- 이름 검색용 인덱스
CREATE INDEX IF NOT EXISTS idx_places_name ON places (name);
