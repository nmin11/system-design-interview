"use client";

import { useState } from "react";

interface SearchBarProps {
  onSearch: (params: {
    type: "coordinates" | "name";
    latitude?: number;
    longitude?: number;
    name?: string;
    radius: number;
  }) => void;
  isLoading?: boolean;
}

export default function SearchBar({ onSearch, isLoading }: SearchBarProps) {
  const [searchType, setSearchType] = useState<"coordinates" | "name">(
    "coordinates"
  );
  const [latitude, setLatitude] = useState("");
  const [longitude, setLongitude] = useState("");
  const [name, setName] = useState("");
  const [radius, setRadius] = useState("3000");

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();

    if (searchType === "coordinates") {
      const lat = parseFloat(latitude);
      const lng = parseFloat(longitude);
      if (isNaN(lat) || isNaN(lng)) {
        alert("올바른 위도/경도를 입력해주세요.");
        return;
      }
      onSearch({
        type: "coordinates",
        latitude: lat,
        longitude: lng,
        radius: parseInt(radius),
      });
    } else {
      if (!name.trim()) {
        alert("검색어를 입력해주세요.");
        return;
      }
      onSearch({
        type: "name",
        name: name.trim(),
        radius: parseInt(radius),
      });
    }
  };

  const handleCurrentLocation = () => {
    if (!navigator.geolocation) {
      alert("브라우저가 위치 정보를 지원하지 않습니다.");
      return;
    }

    navigator.geolocation.getCurrentPosition(
      (position) => {
        setLatitude(position.coords.latitude.toFixed(6));
        setLongitude(position.coords.longitude.toFixed(6));
        setSearchType("coordinates");
      },
      (error) => {
        alert("위치 정보를 가져올 수 없습니다: " + error.message);
      }
    );
  };

  return (
    <form onSubmit={handleSubmit} className="bg-white p-4 rounded-lg shadow-md">
      <div className="flex gap-4 mb-4">
        <button
          type="button"
          onClick={() => setSearchType("coordinates")}
          className={`px-4 py-2 rounded-md text-sm font-medium transition-colors ${
            searchType === "coordinates"
              ? "bg-blue-600 text-white"
              : "bg-gray-100 text-gray-700 hover:bg-gray-200"
          }`}
        >
          위도/경도 검색
        </button>
        <button
          type="button"
          onClick={() => setSearchType("name")}
          className={`px-4 py-2 rounded-md text-sm font-medium transition-colors ${
            searchType === "name"
              ? "bg-blue-600 text-white"
              : "bg-gray-100 text-gray-700 hover:bg-gray-200"
          }`}
        >
          이름 검색
        </button>
      </div>

      {searchType === "coordinates" ? (
        <div className="space-y-3">
          <div className="flex gap-3">
            <div className="flex-1">
              <label className="block text-sm font-medium text-gray-700 mb-1">
                위도 (Latitude)
              </label>
              <input
                type="text"
                value={latitude}
                onChange={(e) => setLatitude(e.target.value)}
                placeholder="37.4979"
                className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 text-gray-900"
              />
            </div>
            <div className="flex-1">
              <label className="block text-sm font-medium text-gray-700 mb-1">
                경도 (Longitude)
              </label>
              <input
                type="text"
                value={longitude}
                onChange={(e) => setLongitude(e.target.value)}
                placeholder="127.0276"
                className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 text-gray-900"
              />
            </div>
          </div>
          <button
            type="button"
            onClick={handleCurrentLocation}
            className="text-sm text-blue-600 hover:text-blue-800"
          >
            현재 위치 사용
          </button>
        </div>
      ) : (
        <div>
          <label className="block text-sm font-medium text-gray-700 mb-1">
            암장 이름
          </label>
          <input
            type="text"
            value={name}
            onChange={(e) => setName(e.target.value)}
            placeholder="클라이밍파크"
            className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 text-gray-900"
          />
        </div>
      )}

      {searchType === "coordinates" && (
        <div className="mt-4">
          <label className="block text-sm font-medium text-gray-700 mb-1">
            검색 반경
          </label>
          <select
            value={radius}
            onChange={(e) => setRadius(e.target.value)}
            className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 text-gray-900"
          >
            <option value="500">0.5km</option>
            <option value="1000">1km</option>
            <option value="2000">2km</option>
            <option value="3000">3km</option>
            <option value="5000">5km</option>
            <option value="10000">10km</option>
            <option value="20000">20km</option>
          </select>
        </div>
      )}

      <button
        type="submit"
        disabled={isLoading}
        className="mt-4 w-full bg-blue-600 text-white py-2 px-4 rounded-md hover:bg-blue-700 disabled:bg-gray-400 transition-colors font-medium"
      >
        {isLoading ? "검색 중..." : "검색"}
      </button>
    </form>
  );
}
