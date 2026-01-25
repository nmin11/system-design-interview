import { useEffect, useRef, useState, useCallback } from "react";
import type { Friend } from "../types/friend";
import type { ClientMessage, ServerMessage } from "../types/message";

const WS_URL = "ws://localhost:8080/ws";
const RECONNECT_DELAY = 3000;

interface UseWebSocketReturn {
  isConnected: boolean;
  friends: Friend[];
  lastUpdate: Date | null;
  sendSearch: (lat: number, lng: number) => void;
  updatePosition: (lat: number, lng: number) => void;
}

export function useWebSocket(): UseWebSocketReturn {
  const [isConnected, setIsConnected] = useState(false);
  const [friends, setFriends] = useState<Friend[]>([]);
  const [lastUpdate, setLastUpdate] = useState<Date | null>(null);
  const wsRef = useRef<WebSocket | null>(null);
  const reconnectTimeoutRef = useRef<number | null>(null);
  const connectRef = useRef<() => void>(undefined);

  useEffect(() => {
    const connect = () => {
      if (wsRef.current?.readyState === WebSocket.OPEN) {
        return;
      }

      const ws = new WebSocket(WS_URL);

      ws.onopen = () => {
        setIsConnected(true);
        console.log("WebSocket connected");
      };

      ws.onmessage = (event) => {
        try {
          const message: ServerMessage = JSON.parse(event.data);
          if (message.type === "friends") {
            setFriends(message.data);
            setLastUpdate(new Date());
          } else if (message.type === "error") {
            console.error("Server error:", message.message);
          }
        } catch (e) {
          console.error("Failed to parse message:", e);
        }
      };

      ws.onclose = () => {
        setIsConnected(false);
        console.log("WebSocket disconnected, reconnecting...");
        reconnectTimeoutRef.current = window.setTimeout(() => {
          connectRef.current?.();
        }, RECONNECT_DELAY);
      };

      ws.onerror = (error) => {
        console.error("WebSocket error:", error);
      };

      wsRef.current = ws;
    };

    connectRef.current = connect;
    connect();

    return () => {
      if (reconnectTimeoutRef.current) {
        clearTimeout(reconnectTimeoutRef.current);
      }
      wsRef.current?.close();
    };
  }, []);

  const sendMessage = useCallback((message: ClientMessage) => {
    if (wsRef.current?.readyState === WebSocket.OPEN) {
      wsRef.current.send(JSON.stringify(message));
    }
  }, []);

  const sendSearch = useCallback(
    (lat: number, lng: number) => {
      sendMessage({ type: "search", lat, lng });
    },
    [sendMessage],
  );

  const updatePosition = useCallback(
    (lat: number, lng: number) => {
      sendMessage({ type: "update_position", lat, lng });
    },
    [sendMessage],
  );

  return {
    isConnected,
    friends,
    lastUpdate,
    sendSearch,
    updatePosition,
  };
}
