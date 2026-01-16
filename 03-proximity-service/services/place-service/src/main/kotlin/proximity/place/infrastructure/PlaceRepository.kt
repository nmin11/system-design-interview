package proximity.place.infrastructure

import org.springframework.data.jpa.repository.JpaRepository
import proximity.place.domain.Place

interface PlaceRepository : JpaRepository<Place, Long>
