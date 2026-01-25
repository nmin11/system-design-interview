import { useCallback, useEffect, useRef, useState } from "react";
import { NaverMap } from "./components/NaverMap";
import { StatusPanel } from "./components/StatusPanel";
import { useWebSocket } from "./hooks/useWebSocket";
import { useGeolocation } from "./hooks/useGeolocation";
import type { Location } from "./types/friend";
import "./App.css";

const DEFAULT_CENTER: Location = {
  lat: 37.5665,
  lng: 126.978,
};

const SEARCH_RADIUS_KM = 8;

function App() {
  const [userLocation, setUserLocation] = useState<Location>(DEFAULT_CENTER);
  const { isConnected, friends, lastUpdate, sendSearch } = useWebSocket();

  const handleLocationChange = useCallback(
    (location: Location) => {
      setUserLocation(location);
      sendSearch(location.lat, location.lng);
    },
    [sendSearch],
  );

  const {
    error: geoError,
    isLoading: geoLoading,
    getCurrentLocation,
  } = useGeolocation({ onSuccess: handleLocationChange });

  // 연결되면 기본 위치에서 검색 시작
  const initialSearchDone = useRef(false);
  useEffect(() => {
    if (isConnected && !initialSearchDone.current) {
      initialSearchDone.current = true;
      sendSearch(userLocation.lat, userLocation.lng);
    }
  }, [isConnected, sendSearch, userLocation.lat, userLocation.lng]);

  const handleMapClick = useCallback(
    (location: Location) => {
      setUserLocation(location);
      sendSearch(location.lat, location.lng);
    },
    [sendSearch],
  );

  return (
    <div className="app-container">
      <NaverMap
        center={userLocation}
        userLocation={userLocation}
        friends={friends}
        radiusKm={SEARCH_RADIUS_KM}
        onMapClick={handleMapClick}
      />
      <StatusPanel
        isConnected={isConnected}
        friendCount={friends.length}
        lastUpdate={lastUpdate}
        userLocation={userLocation}
        onGetLocation={getCurrentLocation}
        isLoadingLocation={geoLoading}
        locationError={geoError}
      />
    </div>
  );
}

export default App;
