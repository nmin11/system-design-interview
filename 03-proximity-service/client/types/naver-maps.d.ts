declare namespace naver.maps {
  class Map {
    constructor(element: HTMLElement | string, options?: MapOptions);
    setCenter(latlng: LatLng): void;
    setZoom(zoom: number): void;
    getCenter(): LatLng;
    panTo(latlng: LatLng): void;
  }

  class LatLng {
    constructor(lat: number, lng: number);
    lat(): number;
    lng(): number;
  }

  class Marker {
    constructor(options: MarkerOptions);
    setMap(map: Map | null): void;
    getPosition(): LatLng;
  }

  class InfoWindow {
    constructor(options: InfoWindowOptions);
    open(map: Map, marker: Marker): void;
    close(): void;
  }

  interface MapOptions {
    center?: LatLng;
    zoom?: number;
    minZoom?: number;
    maxZoom?: number;
  }

  interface MarkerOptions {
    position: LatLng;
    map?: Map;
    title?: string;
    icon?: MarkerIcon;
  }

  interface MarkerIcon {
    url?: string;
    size?: Size;
    origin?: Point;
    anchor?: Point;
  }

  interface InfoWindowOptions {
    content: string | HTMLElement;
    maxWidth?: number;
    borderWidth?: number;
    disableAnchor?: boolean;
  }

  class Size {
    constructor(width: number, height: number);
  }

  class Point {
    constructor(x: number, y: number);
  }

  interface MapClickEvent {
    coord: LatLng;
    point: Point;
    latlng: LatLng;
  }

  class Event {
    static addListener<T = unknown>(
      target: object,
      eventName: string,
      listener: (e: T) => void
    ): void;
  }
}

interface Window {
  naver: typeof naver;
}
