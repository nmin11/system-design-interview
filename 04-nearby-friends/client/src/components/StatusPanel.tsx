import type { Location } from "../types/friend";

interface StatusPanelProps {
  isConnected: boolean;
  friendCount: number;
  lastUpdate: Date | null;
  userLocation: Location;
  onGetLocation: () => void;
  isLoadingLocation: boolean;
  locationError: string | null;
}

export function StatusPanel({
  isConnected,
  friendCount,
  lastUpdate,
  userLocation,
  onGetLocation,
  isLoadingLocation,
  locationError,
}: StatusPanelProps) {
  return (
    <div
      style={{
        position: "absolute",
        top: "10px",
        left: "10px",
        background: "white",
        padding: "15px",
        borderRadius: "8px",
        boxShadow: "0 2px 10px rgba(0,0,0,0.2)",
        zIndex: 1000,
        minWidth: "250px",
      }}
    >
      <h3 style={{ margin: "0 0 10px 0", fontSize: "16px" }}>Nearby Friends</h3>

      <div style={{ marginBottom: "10px" }}>
        <span
          style={{
            display: "inline-block",
            width: "10px",
            height: "10px",
            borderRadius: "50%",
            backgroundColor: isConnected ? "#2ECC71" : "#E74C3C",
            marginRight: "8px",
          }}
        />
        <span style={{ fontSize: "14px" }}>
          {isConnected ? "Connected" : "Disconnected"}
        </span>
      </div>

      <div style={{ fontSize: "14px", marginBottom: "5px" }}>
        <strong>Friends in range:</strong> {friendCount}
      </div>

      {lastUpdate && (
        <div style={{ fontSize: "12px", color: "#666", marginBottom: "10px" }}>
          Last update: {lastUpdate.toLocaleTimeString()}
        </div>
      )}

      <div style={{ fontSize: "12px", color: "#666", marginBottom: "10px" }}>
        Location: {userLocation.lat.toFixed(4)}, {userLocation.lng.toFixed(4)}
      </div>

      <button
        onClick={onGetLocation}
        disabled={isLoadingLocation}
        style={{
          width: "100%",
          padding: "8px 12px",
          backgroundColor: "#4A90D9",
          color: "white",
          border: "none",
          borderRadius: "4px",
          cursor: isLoadingLocation ? "not-allowed" : "pointer",
          fontSize: "14px",
        }}
      >
        {isLoadingLocation ? "Getting location..." : "Use My Location"}
      </button>

      {locationError && (
        <div style={{ fontSize: "12px", color: "#E74C3C", marginTop: "8px" }}>
          {locationError}
        </div>
      )}

      <div
        style={{
          fontSize: "11px",
          color: "#999",
          marginTop: "10px",
          paddingTop: "10px",
          borderTop: "1px solid #eee",
        }}
      >
        Click on the map to search friends at that location
      </div>
    </div>
  );
}
