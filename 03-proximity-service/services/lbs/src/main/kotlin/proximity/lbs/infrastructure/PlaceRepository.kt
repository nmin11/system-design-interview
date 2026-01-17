package proximity.lbs.infrastructure

import org.locationtech.jts.geom.Point
import org.springframework.data.jpa.repository.JpaRepository
import org.springframework.data.jpa.repository.Query
import org.springframework.data.repository.query.Param
import proximity.lbs.domain.Place

interface PlaceRepository : JpaRepository<Place, Long> {

    @Query("""
        SELECT * FROM places p
        WHERE ST_DWithin(
            CAST(p.location AS geography),
            CAST(:point AS geography),
            :radius
        ) = true
        ORDER BY ST_Distance(
            CAST(p.location AS geography),
            CAST(:point AS geography)
        )
    """, nativeQuery = true)
    fun findNearby(
        @Param("point") point: Point,
        @Param("radius") radius: Double
    ): List<Place>

    @Query("""
        SELECT * FROM places p
        ORDER BY ST_Distance(
            CAST(p.location AS geography),
            CAST(:point AS geography)
        )
        LIMIT :limit
    """, nativeQuery = true)
    fun findNearest(
        @Param("point") point: Point,
        @Param("limit") limit: Int
    ): List<Place>

    @Query("""
        SELECT * FROM places p
        WHERE LOWER(p.name) LIKE LOWER(CONCAT('%', :name, '%'))
        ORDER BY p.name
    """, nativeQuery = true)
    fun findByNameContaining(@Param("name") name: String): List<Place>
}
