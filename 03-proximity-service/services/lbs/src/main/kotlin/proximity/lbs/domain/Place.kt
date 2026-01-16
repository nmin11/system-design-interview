package proximity.lbs.domain

import jakarta.persistence.Column
import jakarta.persistence.Entity
import jakarta.persistence.GeneratedValue
import jakarta.persistence.GenerationType
import jakarta.persistence.Id
import jakarta.persistence.Table
import org.locationtech.jts.geom.Point
import java.time.LocalDateTime

@Entity
@Table(name = "places")
class Place(
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    val id: Long = 0,

    @Column(nullable = false)
    val name: String,

    @Column(name = "image_url", nullable = false)
    val imageUrl: String,

    @Column(name = "street_address", nullable = false)
    val streetAddress: String,

    @Column(name = "lot_number", nullable = false)
    val lotNumber: String,

    @Column(columnDefinition = "geometry(Point, 4326)")
    val location: Point,

    @Column(columnDefinition = "TEXT")
    val description: String? = null,

    @Column(name = "instagram_url")
    val instagramUrl: String? = null,

    @Column(name = "phone_number")
    val phoneNumber: String? = null,

    @Column(name = "created_at", nullable = false, updatable = false)
    val createdAt: LocalDateTime = LocalDateTime.now(),

    @Column(name = "updated_at", nullable = false)
    val updatedAt: LocalDateTime = LocalDateTime.now()
)
