declare namespace naver.maps {
  class Map {
    constructor(element: string | HTMLElement, options?: MapOptions);
    setCenter(latlng: LatLng): void;
    getCenter(): LatLng;
    setZoom(zoom: number): void;
    getZoom(): number;
    panTo(latlng: LatLng, options?: PanOptions): void;
  }

  interface MapOptions {
    center?: LatLng;
    zoom?: number;
    minZoom?: number;
    maxZoom?: number;
    zoomControl?: boolean;
    zoomControlOptions?: ZoomControlOptions;
  }

  interface ZoomControlOptions {
    position?: Position;
  }

  interface PanOptions {
    duration?: number;
    easing?: string;
  }

  class LatLng {
    constructor(lat: number, lng: number);
    lat(): number;
    lng(): number;
  }

  class Marker {
    constructor(options: MarkerOptions);
    setMap(map: Map | null): void;
    setPosition(latlng: LatLng): void;
    getPosition(): LatLng;
    setIcon(icon: ImageIcon | SymbolIcon | HtmlIcon): void;
  }

  interface MarkerOptions {
    position: LatLng;
    map?: Map;
    icon?: ImageIcon | SymbolIcon | HtmlIcon;
    title?: string;
    clickable?: boolean;
    zIndex?: number;
  }

  interface ImageIcon {
    url?: string;
    size?: Size;
    scaledSize?: Size;
    origin?: Point;
    anchor?: Point;
  }

  interface SymbolIcon {
    path: SymbolPath | string;
    fillColor?: string;
    fillOpacity?: number;
    strokeColor?: string;
    strokeWeight?: number;
    scale?: number;
    anchor?: Point;
  }

  interface HtmlIcon {
    content: string;
    size?: Size;
    anchor?: Point;
  }

  enum SymbolPath {
    CIRCLE,
    FORWARD_CLOSED_ARROW,
    FORWARD_OPEN_ARROW,
    BACKWARD_CLOSED_ARROW,
    BACKWARD_OPEN_ARROW,
  }

  class Circle {
    constructor(options: CircleOptions);
    setMap(map: Map | null): void;
    setCenter(latlng: LatLng): void;
    setRadius(radius: number): void;
  }

  interface CircleOptions {
    map?: Map;
    center: LatLng;
    radius: number;
    strokeColor?: string;
    strokeOpacity?: number;
    strokeWeight?: number;
    fillColor?: string;
    fillOpacity?: number;
    clickable?: boolean;
  }

  class Size {
    constructor(width: number, height: number);
  }

  class Point {
    constructor(x: number, y: number);
  }

  namespace Event {
    function addListener(
      target: Map | Marker | Circle,
      eventName: string,
      handler: (e: PointerEvent) => void,
    ): MapEventListener;
    function removeListener(listener: MapEventListener): void;
  }

  interface MapEventListener {
    eventName: string;
    target: unknown;
    handler: (e: PointerEvent) => void;
  }

  interface PointerEvent {
    coord: LatLng;
    point: Point;
    offset: Point;
    pointerEvent: MouseEvent;
  }

  enum Position {
    TOP_LEFT,
    TOP_CENTER,
    TOP_RIGHT,
    LEFT_CENTER,
    LEFT_TOP,
    LEFT_BOTTOM,
    RIGHT_TOP,
    RIGHT_CENTER,
    RIGHT_BOTTOM,
    BOTTOM_LEFT,
    BOTTOM_CENTER,
    BOTTOM_RIGHT,
  }
}

interface Window {
  naver: typeof naver;
}
