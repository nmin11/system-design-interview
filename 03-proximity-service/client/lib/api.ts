const LBS_API_URL =
  process.env.NEXT_PUBLIC_LBS_API_URL || "http://localhost:8081";

export interface Place {
  id: number;
  name: string;
  imageUrl: string;
  streetAddress: string;
  lotNumber: string;
  latitude: number;
  longitude: number;
  description: string | null;
  instagramUrl: string | null;
  phoneNumber: string | null;
}

export interface NearbyResponse {
  places: Place[];
  count: number;
  searchLatitude: number;
  searchLongitude: number;
  radiusMeters: number;
}

export interface NearestResponse {
  places: Place[];
  count: number;
  searchLatitude: number;
  searchLongitude: number;
}

export interface SearchResponse {
  places: Place[];
  count: number;
  searchName: string;
}

export async function searchNearby(
  latitude: number,
  longitude: number,
  radius: number = 3000
): Promise<NearbyResponse> {
  const response = await fetch(
    `${LBS_API_URL}/nearby?latitude=${latitude}&longitude=${longitude}&radius=${radius}`
  );
  if (!response.ok) {
    throw new Error("Failed to fetch nearby places");
  }
  return response.json();
}

export async function searchNearest(
  latitude: number,
  longitude: number,
  limit: number = 10
): Promise<NearestResponse> {
  const response = await fetch(
    `${LBS_API_URL}/nearest?latitude=${latitude}&longitude=${longitude}&limit=${limit}`
  );
  if (!response.ok) {
    throw new Error("Failed to fetch nearest places");
  }
  return response.json();
}

export async function searchByName(name: string): Promise<SearchResponse> {
  const response = await fetch(
    `${LBS_API_URL}/search?name=${encodeURIComponent(name)}`
  );
  if (!response.ok) {
    throw new Error("Failed to search places by name");
  }
  return response.json();
}
