package proximity.place.domain

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.GeometryFactory
import org.locationtech.jts.geom.PrecisionModel
import org.springframework.stereotype.Service
import org.springframework.transaction.annotation.Transactional
import proximity.place.infrastructure.PlaceRepository

@Service
@Transactional(readOnly = true)
class PlaceService(
    private val placeRepository: PlaceRepository
) {
    private val geometryFactory = GeometryFactory(PrecisionModel(), 4326)

    fun findAll(): List<Place> = placeRepository.findAll()

    fun findById(id: Long): Place =
        placeRepository.findById(id).orElseThrow {
            NoSuchElementException("Place not found with id: $id")
        }

    @Transactional
    fun create(
        name: String,
        imageUrl: String,
        streetAddress: String,
        lotNumber: String,
        latitude: Double,
        longitude: Double,
        description: String?,
        instagramUrl: String?,
        phoneNumber: String?
    ): Place {
        val location = geometryFactory.createPoint(Coordinate(longitude, latitude))
        val place = Place(
            name = name,
            imageUrl = imageUrl,
            streetAddress = streetAddress,
            lotNumber = lotNumber,
            location = location,
            description = description,
            instagramUrl = instagramUrl,
            phoneNumber = phoneNumber
        )
        return placeRepository.save(place)
    }

    @Transactional
    fun update(
        id: Long,
        name: String?,
        imageUrl: String?,
        streetAddress: String?,
        lotNumber: String?,
        latitude: Double?,
        longitude: Double?,
        description: String?,
        instagramUrl: String?,
        phoneNumber: String?
    ): Place {
        val place = findById(id)

        name?.let { place.name = it }
        imageUrl?.let { place.imageUrl = it }
        streetAddress?.let { place.streetAddress = it }
        lotNumber?.let { place.lotNumber = it }
        description?.let { place.description = it }
        instagramUrl?.let { place.instagramUrl = it }
        phoneNumber?.let { place.phoneNumber = it }

        if (latitude != null && longitude != null) {
            place.location = geometryFactory.createPoint(Coordinate(longitude, latitude))
        }

        return placeRepository.save(place)
    }

    @Transactional
    fun delete(id: Long) {
        val place = findById(id)
        placeRepository.delete(place)
    }
}
