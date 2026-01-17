"use client";

import type { Place } from "@/lib/api";

interface PlaceListProps {
  places: Place[];
  selectedPlace?: Place | null;
  onPlaceClick: (place: Place) => void;
}

export default function PlaceList({
  places,
  selectedPlace,
  onPlaceClick,
}: PlaceListProps) {
  if (places.length === 0) {
    return (
      <div className="bg-white p-6 rounded-lg shadow-md text-center text-gray-500">
        검색 결과가 없습니다.
      </div>
    );
  }

  return (
    <div className="bg-white rounded-lg shadow-md overflow-hidden">
      <div className="p-4 border-b border-gray-200">
        <h2 className="font-semibold text-gray-900">
          검색 결과 ({places.length}개)
        </h2>
      </div>
      <ul className="divide-y divide-gray-200 max-h-[500px] overflow-y-auto">
        {places.map((place) => (
          <li
            key={place.id}
            onClick={() => onPlaceClick(place)}
            className={`p-4 cursor-pointer transition-colors hover:bg-gray-50 ${
              selectedPlace?.id === place.id ? "bg-blue-50" : ""
            }`}
          >
            <div className="flex gap-3">
              {place.imageUrl && (
                <img
                  src={place.imageUrl}
                  alt={place.name}
                  className="w-16 h-16 object-cover rounded-md flex-shrink-0"
                />
              )}
              <div className="flex-1 min-w-0">
                <h3 className="font-medium text-gray-900">{place.name}</h3>
                <p className="text-sm text-gray-600 mt-1 truncate">
                  {place.streetAddress}
                </p>
                {place.phoneNumber && (
                  <p className="text-sm text-gray-500 mt-1">{place.phoneNumber}</p>
                )}
                {place.instagramUrl && (
                  <a
                    href={place.instagramUrl}
                    target="_blank"
                    rel="noopener noreferrer"
                    onClick={(e) => e.stopPropagation()}
                    className="text-xs text-pink-600 hover:underline mt-1 inline-block"
                  >
                    Instagram
                  </a>
                )}
              </div>
            </div>
          </li>
        ))}
      </ul>
    </div>
  );
}
