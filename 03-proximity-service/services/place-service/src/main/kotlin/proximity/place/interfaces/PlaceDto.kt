package proximity.place.interfaces

import proximity.place.domain.Place
import java.time.LocalDateTime

data class CreatePlaceRequest(
    val name: String,
    val imageUrl: String,
    val streetAddress: String,
    val lotNumber: String,
    val latitude: Double,
    val longitude: Double,
    val description: String? = null,
    val instagramUrl: String? = null,
    val phoneNumber: String? = null
)

data class UpdatePlaceRequest(
    val name: String? = null,
    val imageUrl: String? = null,
    val streetAddress: String? = null,
    val lotNumber: String? = null,
    val latitude: Double? = null,
    val longitude: Double? = null,
    val description: String? = null,
    val instagramUrl: String? = null,
    val phoneNumber: String? = null
)

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
    val phoneNumber: String?,
    val createdAt: LocalDateTime,
    val updatedAt: LocalDateTime
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
            phoneNumber = place.phoneNumber,
            createdAt = place.createdAt,
            updatedAt = place.updatedAt
        )
    }
}
