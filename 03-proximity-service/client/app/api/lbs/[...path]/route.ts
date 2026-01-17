import { NextRequest, NextResponse } from "next/server";

const LBS_API_URL = process.env.LBS_API_URL || "http://localhost:8081";

export async function GET(
  request: NextRequest,
  { params }: { params: Promise<{ path: string[] }> }
) {
  const { path } = await params;
  const pathname = path.join("/");
  const searchParams = request.nextUrl.searchParams.toString();
  const url = `${LBS_API_URL}/${pathname}${searchParams ? `?${searchParams}` : ""}`;

  try {
    const response = await fetch(url);
    const data = await response.json();
    return NextResponse.json(data);
  } catch (error) {
    console.error("LBS API error:", error);
    return NextResponse.json(
      { error: "Failed to fetch from LBS API" },
      { status: 500 }
    );
  }
}
