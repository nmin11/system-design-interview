package proximity.lbs.interfaces

import proximity.lbs.domain.Place

data class PlaceResponse(
    val id: Long,
    val name: String,
    val imageUrl: String,
    val streetAddress: String,
    val lotNumber: String,
    val latitude: Double,
    val longitude: Double,
    val description: String?,
    val instagramUrl: String?,
    val phoneNumber: String?
) {
    companion object {
        fun from(place: Place): PlaceResponse = PlaceResponse(
            id = place.id,
            name = place.name,
            imageUrl = place.imageUrl,
            streetAddress = place.streetAddress,
            lotNumber = place.lotNumber,
            latitude = place.location.y,
            longitude = place.location.x,
            description = place.description,
            instagramUrl = place.instagramUrl,
            phoneNumber = place.phoneNumber
        )
    }
}

data class NearbyResponse(
    val places: List<PlaceResponse>,
    val count: Int,
    val searchLatitude: Double,
    val searchLongitude: Double,
    val radiusMeters: Double
)

data class NearestResponse(
    val places: List<PlaceResponse>,
    val count: Int,
    val searchLatitude: Double,
    val searchLongitude: Double
)
