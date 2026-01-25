import type { Friend } from "./friend";

export type ClientMessage =
  | { type: "search"; lat: number; lng: number }
  | { type: "update_position"; lat: number; lng: number };

export type ServerMessage =
  | { type: "friends"; data: Friend[] }
  | { type: "error"; message: string };
