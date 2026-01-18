"use client";

import { useEffect, useRef, useCallback } from "react";
import type { Place } from "@/lib/api";

interface NaverMapProps {
  places: Place[];
  center: { lat: number; lng: number };
  searchLocation?: { lat: number; lng: number } | null;
  searchRadius?: number | null;
  selectedPlace?: Place | null;
  onMapClick?: (lat: number, lng: number) => void;
  onPlaceSelect?: (place: Place) => void;
}

export default function NaverMap({
  places,
  center,
  searchLocation,
  searchRadius,
  selectedPlace,
  onMapClick,
  onPlaceSelect,
}: NaverMapProps) {
  const mapRef = useRef<HTMLDivElement>(null);
  const mapInstanceRef = useRef<naver.maps.Map | null>(null);
  const markersRef = useRef<naver.maps.Marker[]>([]);
  const searchMarkerRef = useRef<naver.maps.Marker | null>(null);
  const searchCircleRef = useRef<naver.maps.Circle | null>(null);
  const selectedMarkerRef = useRef<naver.maps.Marker | null>(null);
  const infoWindowRef = useRef<naver.maps.InfoWindow | null>(null);

  const clearMarkers = useCallback(() => {
    markersRef.current.forEach((marker) => marker.setMap(null));
    markersRef.current = [];
  }, []);

  const createInfoWindowContent = useCallback((place: Place) => {
    return `
      <div style="padding: 16px; min-width: 280px; max-width: 320px;">
        ${
          place.imageUrl
            ? `<img src="${place.imageUrl}" alt="${place.name}" style="width: 100%; height: 120px; object-fit: cover; border-radius: 8px; margin-bottom: 12px;" />`
            : ""
        }
        <h3 style="margin: 0 0 8px 0; font-size: 16px; font-weight: 600; color: #000;">${
          place.name
        }</h3>
        <p style="margin: 0 0 4px 0; font-size: 13px; color: #666;">${
          place.streetAddress
        }</p>
        ${
          place.phoneNumber
            ? `<p style="margin: 0 0 4px 0; font-size: 13px; color: #666;">Tel: ${place.phoneNumber}</p>`
            : ""
        }
        ${
          place.description
            ? `<p style="margin: 8px 0 0 0; font-size: 12px; color: #888; line-height: 1.4;">${place.description.slice(
                0,
                100
              )}${place.description.length > 100 ? "..." : ""}</p>`
            : ""
        }
        ${
          place.instagramUrl
            ? `<a href="${place.instagramUrl}" target="_blank" style="display: inline-block; margin-top: 8px; font-size: 12px; color: #E1306C;">Instagram</a>`
            : ""
        }
      </div>
    `;
  }, []);

  // 지도 초기화
  useEffect(() => {
    if (!mapRef.current || !window.naver) return;

    const mapOptions = {
      center: new window.naver.maps.LatLng(center.lat, center.lng),
      zoom: 14,
      minZoom: 10,
      maxZoom: 19,
    };

    mapInstanceRef.current = new window.naver.maps.Map(
      mapRef.current,
      mapOptions
    );

    // 지도 클릭 이벤트
    if (onMapClick) {
      window.naver.maps.Event.addListener<naver.maps.MapClickEvent>(
        mapInstanceRef.current,
        "click",
        (e) => {
          onMapClick(e.coord.lat(), e.coord.lng());
        }
      );
    }

    return () => {
      clearMarkers();
    };
  }, [center.lat, center.lng, onMapClick, clearMarkers]);

  // 마커 업데이트
  useEffect(() => {
    if (!mapInstanceRef.current || !window.naver) return;

    clearMarkers();

    if (infoWindowRef.current) {
      infoWindowRef.current.close();
    }

    infoWindowRef.current = new window.naver.maps.InfoWindow({
      content: "",
      maxWidth: 320,
      borderWidth: 0,
      disableAnchor: false,
    });

    places.forEach((place) => {
      const marker = new window.naver.maps.Marker({
        position: new window.naver.maps.LatLng(place.latitude, place.longitude),
        map: mapInstanceRef.current!,
        title: place.name,
      });

      window.naver.maps.Event.addListener(marker, "click", () => {
        if (infoWindowRef.current && mapInstanceRef.current) {
          infoWindowRef.current.close();
          infoWindowRef.current = new window.naver.maps.InfoWindow({
            content: createInfoWindowContent(place),
            maxWidth: 320,
            borderWidth: 0,
          });
          infoWindowRef.current.open(mapInstanceRef.current, marker);
        }
        onPlaceSelect?.(place);
      });

      markersRef.current.push(marker);
    });
  }, [places, clearMarkers, createInfoWindowContent, onPlaceSelect]);

  // 센터 변경
  useEffect(() => {
    if (mapInstanceRef.current && window.naver) {
      mapInstanceRef.current.panTo(
        new window.naver.maps.LatLng(center.lat, center.lng)
      );
    }
  }, [center]);

  // 검색 위치 마커 및 반경 원
  useEffect(() => {
    if (!mapInstanceRef.current || !window.naver) return;

    // 기존 검색 마커 제거
    if (searchMarkerRef.current) {
      searchMarkerRef.current.setMap(null);
      searchMarkerRef.current = null;
    }

    // 기존 검색 반경 원 제거
    if (searchCircleRef.current) {
      searchCircleRef.current.setMap(null);
      searchCircleRef.current = null;
    }

    if (searchLocation) {
      // 검색 반경 원 그리기
      if (searchRadius) {
        searchCircleRef.current = new window.naver.maps.Circle({
          map: mapInstanceRef.current,
          center: new window.naver.maps.LatLng(
            searchLocation.lat,
            searchLocation.lng
          ),
          radius: searchRadius,
          fillColor: "#3B82F6",
          fillOpacity: 0.1,
          strokeColor: "#3B82F6",
          strokeOpacity: 0.6,
          strokeWeight: 2,
        });
      }

      // 검색 위치 마커
      searchMarkerRef.current = new window.naver.maps.Marker({
        position: new window.naver.maps.LatLng(
          searchLocation.lat,
          searchLocation.lng
        ),
        map: mapInstanceRef.current,
        icon: {
          content: `
            <div style="
              width: 24px;
              height: 24px;
              background: #3B82F6;
              border: 3px solid #1D4ED8;
              border-radius: 50%;
              box-shadow: 0 2px 6px rgba(0,0,0,0.3);
            "></div>
          `,
          anchor: new window.naver.maps.Point(12, 12),
        },
        title: "검색 위치",
      });
    }
  }, [searchLocation, searchRadius]);

  // 선택된 암장 마커
  useEffect(() => {
    if (!mapInstanceRef.current || !window.naver) return;

    // 기존 선택 마커 제거
    if (selectedMarkerRef.current) {
      selectedMarkerRef.current.setMap(null);
      selectedMarkerRef.current = null;
    }

    if (selectedPlace) {
      selectedMarkerRef.current = new window.naver.maps.Marker({
        position: new window.naver.maps.LatLng(
          selectedPlace.latitude,
          selectedPlace.longitude
        ),
        map: mapInstanceRef.current,
        icon: {
          content: `
            <div style="
              width: 32px;
              height: 32px;
              background: #EF4444;
              border: 3px solid #B91C1C;
              border-radius: 50%;
              box-shadow: 0 2px 8px rgba(0,0,0,0.4);
              display: flex;
              align-items: center;
              justify-content: center;
            ">
              <div style="
                width: 8px;
                height: 8px;
                background: white;
                border-radius: 50%;
              "></div>
            </div>
          `,
          anchor: new window.naver.maps.Point(16, 16),
        },
        title: selectedPlace.name,
        zIndex: 1000,
      });

      // 선택된 암장의 InfoWindow 표시
      if (infoWindowRef.current) {
        infoWindowRef.current.close();
      }
      infoWindowRef.current = new window.naver.maps.InfoWindow({
        content: createInfoWindowContent(selectedPlace),
        maxWidth: 320,
        borderWidth: 0,
      });
      infoWindowRef.current.open(mapInstanceRef.current, selectedMarkerRef.current);
    }
  }, [selectedPlace, createInfoWindowContent]);

  return (
    <div
      ref={mapRef}
      className="w-full h-full min-h-[400px] rounded-lg overflow-hidden"
    />
  );
}
