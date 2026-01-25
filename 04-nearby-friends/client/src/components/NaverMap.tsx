import { useEffect, useRef, useState } from "react";
import type { Friend, Location } from "../types/friend";

interface NaverMapProps {
  center: Location;
  userLocation: Location;
  friends: Friend[];
  radiusKm: number;
  onMapClick: (location: Location) => void;
}

const FRIEND_MARKER_COLOR = "#4A90D9";
const USER_MARKER_COLOR = "#E74C3C";
const RADIUS_COLOR = "#4A90D9";

export function NaverMap({
  center,
  userLocation,
  friends,
  radiusKm,
  onMapClick,
}: NaverMapProps) {
  const [mapReady, setMapReady] = useState(false);
  const mapRef = useRef<naver.maps.Map | null>(null);
  const userMarkerRef = useRef<naver.maps.Marker | null>(null);
  const radiusCircleRef = useRef<naver.maps.Circle | null>(null);
  const friendMarkersRef = useRef<Map<string, naver.maps.Marker>>(new Map());
  const mapContainerRef = useRef<HTMLDivElement>(null);

  const onMapClickRef = useRef(onMapClick);
  const initialCenterRef = useRef(center);

  useEffect(() => {
    onMapClickRef.current = onMapClick;
  }, [onMapClick]);

  useEffect(() => {
    if (mapRef.current) return; // 이미 초기화됨

    const initialCenter = initialCenterRef.current;
    const checkNaverMaps = setInterval(() => {
      if (window.naver?.maps && mapContainerRef.current) {
        clearInterval(checkNaverMaps);

        const map = new window.naver.maps.Map(mapContainerRef.current, {
          center: new window.naver.maps.LatLng(initialCenter.lat, initialCenter.lng),
          zoom: 11,
          zoomControl: true,
          zoomControlOptions: {
            position: window.naver.maps.Position.TOP_RIGHT,
          },
        });

        window.naver.maps.Event.addListener(
          map,
          "click",
          (e: naver.maps.PointerEvent) => {
            onMapClickRef.current({
              lat: e.coord.lat(),
              lng: e.coord.lng(),
            });
          },
        );

        mapRef.current = map;
        setMapReady(true);
      }
    }, 100);

    return () => {
      clearInterval(checkNaverMaps);
    };
  }, []);

  // 사용자 마커와 반경 원 그리기
  useEffect(() => {
    if (!mapReady || !mapRef.current || !window.naver?.maps) return;

    const position = new window.naver.maps.LatLng(
      userLocation.lat,
      userLocation.lng,
    );

    // 사용자 마커
    if (!userMarkerRef.current) {
      userMarkerRef.current = new window.naver.maps.Marker({
        position,
        map: mapRef.current,
        icon: {
          content: `<div style="
            width: 24px;
            height: 24px;
            background-color: ${USER_MARKER_COLOR};
            border: 3px solid white;
            border-radius: 50%;
            box-shadow: 0 2px 6px rgba(0,0,0,0.3);
          "></div>`,
          anchor: new window.naver.maps.Point(12, 12),
        },
        zIndex: 1000,
      });
    } else {
      userMarkerRef.current.setPosition(position);
    }

    // 8km 반경 원
    const radiusMeters = radiusKm * 1000;
    if (!radiusCircleRef.current) {
      radiusCircleRef.current = new window.naver.maps.Circle({
        map: mapRef.current,
        center: position,
        radius: radiusMeters,
        strokeColor: RADIUS_COLOR,
        strokeOpacity: 0.8,
        strokeWeight: 2,
        fillColor: RADIUS_COLOR,
        fillOpacity: 0.15,
        clickable: false,
      });
    } else {
      radiusCircleRef.current.setCenter(position);
      radiusCircleRef.current.setRadius(radiusMeters);
    }

    mapRef.current.panTo(position, { duration: 300 });
  }, [mapReady, userLocation, radiusKm]);

  // 친구 마커 그리기
  useEffect(() => {
    if (!mapReady || !mapRef.current || !window.naver?.maps) return;

    const currentFriendIds = new Set(friends.map((f) => f.id));

    // 범위를 벗어난 친구 마커 제거
    friendMarkersRef.current.forEach((marker, id) => {
      if (!currentFriendIds.has(id)) {
        marker.setMap(null);
        friendMarkersRef.current.delete(id);
      }
    });

    // 친구 마커 추가/업데이트
    friends.forEach((friend) => {
      const position = new window.naver.maps.LatLng(friend.lat, friend.lng);
      const existingMarker = friendMarkersRef.current.get(friend.id);

      if (existingMarker) {
        existingMarker.setPosition(position);
      } else {
        const marker = new window.naver.maps.Marker({
          position,
          map: mapRef.current!,
          icon: {
            content: `<div style="
              width: 14px;
              height: 14px;
              background-color: ${FRIEND_MARKER_COLOR};
              border: 2px solid white;
              border-radius: 50%;
              box-shadow: 0 1px 4px rgba(0,0,0,0.3);
            "></div>`,
            anchor: new window.naver.maps.Point(7, 7),
          },
          title: `${friend.id} (${friend.distance.toFixed(2)}km)`,
          zIndex: 100,
        });
        friendMarkersRef.current.set(friend.id, marker);
      }
    });
  }, [mapReady, friends]);

  return (
    <div
      ref={mapContainerRef}
      style={{
        width: "100%",
        height: "100%",
        minHeight: "400px",
      }}
    />
  );
}
