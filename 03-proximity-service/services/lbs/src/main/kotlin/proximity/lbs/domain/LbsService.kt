package proximity.lbs.domain

import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.GeometryFactory
import org.locationtech.jts.geom.PrecisionModel
import org.springframework.stereotype.Service
import org.springframework.transaction.annotation.Transactional
import proximity.lbs.infrastructure.PlaceRepository

@Service
@Transactional(readOnly = true)
class LbsService(
    private val placeRepository: PlaceRepository
) {
    private val geometryFactory = GeometryFactory(PrecisionModel(), 4326)

    fun findNearby(latitude: Double, longitude: Double, radiusMeters: Double): List<Place> {
        val point = geometryFactory.createPoint(Coordinate(longitude, latitude))
        return placeRepository.findNearby(point, radiusMeters)
    }

    fun findNearest(latitude: Double, longitude: Double, limit: Int): List<Place> {
        val point = geometryFactory.createPoint(Coordinate(longitude, latitude))
        return placeRepository.findNearest(point, limit)
    }

    fun searchByName(name: String): List<Place> {
        return placeRepository.findByNameContaining(name)
    }
}
