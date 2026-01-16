"use client";

import { useEffect, useRef, useCallback } from "react";
import type { Place } from "@/lib/api";

interface NaverMapProps {
  places: Place[];
  center: { lat: number; lng: number };
  onMapClick?: (lat: number, lng: number) => void;
  onPlaceSelect?: (place: Place) => void;
}

export default function NaverMap({
  places,
  center,
  onMapClick,
  onPlaceSelect,
}: NaverMapProps) {
  const mapRef = useRef<HTMLDivElement>(null);
  const mapInstanceRef = useRef<naver.maps.Map | null>(null);
  const markersRef = useRef<naver.maps.Marker[]>([]);
  const infoWindowRef = useRef<naver.maps.InfoWindow | null>(null);

  const clearMarkers = useCallback(() => {
    markersRef.current.forEach((marker) => marker.setMap(null));
    markersRef.current = [];
  }, []);

  const createInfoWindowContent = useCallback((place: Place) => {
    return `
      <div style="padding: 16px; min-width: 250px; max-width: 300px;">
        <h3 style="margin: 0 0 8px 0; font-size: 16px; font-weight: 600;">${
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

  return (
    <div
      ref={mapRef}
      className="w-full h-full min-h-[400px] rounded-lg overflow-hidden"
    />
  );
}
