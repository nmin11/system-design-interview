package proximity.place.interfaces

import org.springframework.http.HttpStatus
import org.springframework.http.ResponseEntity
import org.springframework.web.bind.annotation.*
import proximity.place.domain.PlaceService

@RestController
@RequestMapping("/places")
class PlaceController(
    private val placeService: PlaceService
) {
    @GetMapping
    fun findAll(): ResponseEntity<List<PlaceResponse>> {
        val places = placeService.findAll().map { PlaceResponse.from(it) }
        return ResponseEntity.ok(places)
    }

    @GetMapping("/{id}")
    fun findById(@PathVariable id: Long): ResponseEntity<PlaceResponse> {
        val place = placeService.findById(id)
        return ResponseEntity.ok(PlaceResponse.from(place))
    }

    @PostMapping
    fun create(@RequestBody request: CreatePlaceRequest): ResponseEntity<PlaceResponse> {
        val place = placeService.create(
            name = request.name,
            imageUrl = request.imageUrl,
            streetAddress = request.streetAddress,
            lotNumber = request.lotNumber,
            latitude = request.latitude,
            longitude = request.longitude,
            description = request.description,
            instagramUrl = request.instagramUrl,
            phoneNumber = request.phoneNumber
        )
        return ResponseEntity.status(HttpStatus.CREATED).body(PlaceResponse.from(place))
    }

    @PutMapping("/{id}")
    fun update(
        @PathVariable id: Long,
        @RequestBody request: UpdatePlaceRequest
    ): ResponseEntity<PlaceResponse> {
        val place = placeService.update(
            id = id,
            name = request.name,
            imageUrl = request.imageUrl,
            streetAddress = request.streetAddress,
            lotNumber = request.lotNumber,
            latitude = request.latitude,
            longitude = request.longitude,
            description = request.description,
            instagramUrl = request.instagramUrl,
            phoneNumber = request.phoneNumber
        )
        return ResponseEntity.ok(PlaceResponse.from(place))
    }

    @DeleteMapping("/{id}")
    fun delete(@PathVariable id: Long): ResponseEntity<Unit> {
        placeService.delete(id)
        return ResponseEntity.noContent().build()
    }
}
