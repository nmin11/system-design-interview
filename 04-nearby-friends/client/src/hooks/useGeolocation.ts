import { useCallback, useState } from "react";
import type { Location } from "../types/friend";

interface UseGeolocationOptions {
  onSuccess?: (location: Location) => void;
}

interface UseGeolocationReturn {
  error: string | null;
  isLoading: boolean;
  getCurrentLocation: () => void;
}

export function useGeolocation(options?: UseGeolocationOptions): UseGeolocationReturn {
  const [error, setError] = useState<string | null>(null);
  const [isLoading, setIsLoading] = useState(false);

  const getCurrentLocation = useCallback(() => {
    if (!navigator.geolocation) {
      setError("Geolocation is not supported by this browser");
      return;
    }

    setIsLoading(true);
    setError(null);

    navigator.geolocation.getCurrentPosition(
      (position) => {
        const location = {
          lat: position.coords.latitude,
          lng: position.coords.longitude,
        };
        options?.onSuccess?.(location);
        setIsLoading(false);
      },
      (err) => {
        let errorMessage: string;
        switch (err.code) {
          case err.PERMISSION_DENIED:
            errorMessage = "Location permission denied";
            break;
          case err.POSITION_UNAVAILABLE:
            errorMessage = "Location information unavailable";
            break;
          case err.TIMEOUT:
            errorMessage = "Location request timed out";
            break;
          default:
            errorMessage = "An unknown error occurred";
        }
        setError(errorMessage);
        setIsLoading(false);
      },
      {
        enableHighAccuracy: true,
        timeout: 10000,
        maximumAge: 0,
      },
    );
  }, [options]);

  return {
    error,
    isLoading,
    getCurrentLocation,
  };
}
