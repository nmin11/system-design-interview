-- PostGIS 확장 활성화
CREATE EXTENSION IF NOT EXISTS postgis;
CREATE EXTENSION IF NOT EXISTS postgis_topology;

-- 설치된 PostGIS 버전 확인
SELECT PostGIS_Full_Version();
