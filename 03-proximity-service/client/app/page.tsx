"use client";

import { useState, useCallback } from "react";
import NaverMap from "@/components/NaverMap";
import SearchBar from "@/components/SearchBar";
import PlaceList from "@/components/PlaceList";
import { searchNearby, type Place } from "@/lib/api";

// 서울 중심 좌표 (강남역)
const DEFAULT_CENTER = { lat: 37.4979, lng: 127.0276 };

export default function Home() {
  const [places, setPlaces] = useState<Place[]>([]);
  const [center, setCenter] = useState(DEFAULT_CENTER);
  const [selectedPlace, setSelectedPlace] = useState<Place | null>(null);
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const handleSearch = useCallback(
    async (params: {
      type: "coordinates" | "name";
      latitude?: number;
      longitude?: number;
      name?: string;
      radius: number;
    }) => {
      setIsLoading(true);
      setError(null);

      try {
        if (
          params.type === "coordinates" &&
          params.latitude &&
          params.longitude
        ) {
          const response = await searchNearby(
            params.latitude,
            params.longitude,
            params.radius
          );
          setPlaces(response.places);
          setCenter({ lat: params.latitude, lng: params.longitude });
        } else if (params.type === "name" && params.name) {
          // 이름 검색의 경우 현재 중심 좌표 기준으로 검색 후 필터링
          const response = await searchNearby(
            center.lat,
            center.lng,
            params.radius
          );
          const filtered = response.places.filter((place) =>
            place.name.toLowerCase().includes(params.name!.toLowerCase())
          );
          setPlaces(filtered);
        }
      } catch (err) {
        setError("검색 중 오류가 발생했습니다. 서버 연결을 확인해주세요.");
        console.error(err);
      } finally {
        setIsLoading(false);
      }
    },
    [center]
  );

  const handleMapClick = useCallback((lat: number, lng: number) => {
    setCenter({ lat, lng });
  }, []);

  const handlePlaceSelect = useCallback((place: Place) => {
    setSelectedPlace(place);
    setCenter({ lat: place.latitude, lng: place.longitude });
  }, []);

  return (
    <div className="min-h-screen bg-gray-100">
      <header className="bg-white shadow-sm">
        <div className="max-w-7xl mx-auto px-4 py-4">
          <h1 className="text-2xl font-bold text-gray-900">
            클라이밍 암장 찾기
          </h1>
          <p className="text-sm text-gray-600 mt-1">
            내 주변 클라이밍 암장을 검색해보세요
          </p>
        </div>
      </header>

      <main className="max-w-7xl mx-auto px-4 py-6">
        <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
          {/* 왼쪽: 검색 + 목록 */}
          <div className="lg:col-span-1 space-y-4">
            <SearchBar onSearch={handleSearch} isLoading={isLoading} />

            {error && (
              <div className="bg-red-50 border border-red-200 text-red-700 px-4 py-3 rounded-lg">
                {error}
              </div>
            )}

            <PlaceList
              places={places}
              selectedPlace={selectedPlace}
              onPlaceClick={handlePlaceSelect}
            />
          </div>

          {/* 오른쪽: 지도 */}
          <div className="lg:col-span-2">
            <div className="bg-white rounded-lg shadow-md p-2 h-[600px]">
              <NaverMap
                places={places}
                center={center}
                onMapClick={handleMapClick}
                onPlaceSelect={setSelectedPlace}
              />
            </div>
            <p className="text-xs text-gray-500 mt-2 text-center">
              지도를 클릭하면 해당 위치로 중심이 이동합니다
            </p>
          </div>
        </div>
      </main>
    </div>
  );
}
