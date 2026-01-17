package proximity.lbs.interfaces

import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.RequestParam
import org.springframework.web.bind.annotation.RestController
import proximity.lbs.domain.LbsService

@RestController
class LbsController(
    private val lbsService: LbsService
) {
    @GetMapping("/nearby")
    fun findNearby(
        @RequestParam latitude: Double,
        @RequestParam longitude: Double,
        @RequestParam(defaultValue = "1000") radius: Double
    ): ResponseEntity<NearbyResponse> {
        val places = lbsService.findNearby(latitude, longitude, radius)
        val response = NearbyResponse(
            places = places.map { PlaceResponse.from(it) },
            count = places.size,
            searchLatitude = latitude,
            searchLongitude = longitude,
            radiusMeters = radius
        )
        return ResponseEntity.ok(response)
    }

    @GetMapping("/nearest")
    fun findNearest(
        @RequestParam latitude: Double,
        @RequestParam longitude: Double,
        @RequestParam(defaultValue = "5") limit: Int
    ): ResponseEntity<NearestResponse> {
        val places = lbsService.findNearest(latitude, longitude, limit)
        val response = NearestResponse(
            places = places.map { PlaceResponse.from(it) },
            count = places.size,
            searchLatitude = latitude,
            searchLongitude = longitude
        )
        return ResponseEntity.ok(response)
    }

    @GetMapping("/search")
    fun searchByName(
        @RequestParam name: String
    ): ResponseEntity<SearchResponse> {
        val places = lbsService.searchByName(name)
        val response = SearchResponse(
            places = places.map { PlaceResponse.from(it) },
            count = places.size,
            searchName = name
        )
        return ResponseEntity.ok(response)
    }
}
