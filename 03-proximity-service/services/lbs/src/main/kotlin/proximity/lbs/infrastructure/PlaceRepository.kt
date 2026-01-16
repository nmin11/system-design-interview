package proximity.lbs.infrastructure

import org.locationtech.jts.geom.Point
import org.springframework.data.jpa.repository.JpaRepository
import org.springframework.data.jpa.repository.Query
import org.springframework.data.repository.query.Param
import proximity.lbs.domain.Place

interface PlaceRepository : JpaRepository<Place, Long> {

    @Query("""
        SELECT p FROM Place p
        WHERE ST_DWithin(p.location, :point, :radius) = true
        ORDER BY ST_Distance(p.location, :point)
    """)
    fun findNearby(
        @Param("point") point: Point,
        @Param("radius") radius: Double
    ): List<Place>

    @Query("""
        SELECT p FROM Place p
        ORDER BY ST_Distance(p.location, :point)
        LIMIT :limit
    """)
    fun findNearest(
        @Param("point") point: Point,
        @Param("limit") limit: Int
    ): List<Place>
}
