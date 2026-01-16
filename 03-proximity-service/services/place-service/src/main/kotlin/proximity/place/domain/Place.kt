package proximity.place.domain

import jakarta.persistence.Column
import jakarta.persistence.Entity
import jakarta.persistence.GeneratedValue
import jakarta.persistence.GenerationType
import jakarta.persistence.Id
import jakarta.persistence.PreUpdate
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
    var name: String,

    @Column(name = "image_url", nullable = false)
    var imageUrl: String,

    @Column(name = "street_address", nullable = false)
    var streetAddress: String,

    @Column(name = "lot_number", nullable = false)
    var lotNumber: String,

    @Column(columnDefinition = "geometry(Point, 4326)")
    var location: Point,

    @Column(columnDefinition = "TEXT")
    var description: String? = null,

    @Column(name = "instagram_url")
    var instagramUrl: String? = null,

    @Column(name = "phone_number")
    var phoneNumber: String? = null,

    @Column(name = "created_at", nullable = false, updatable = false)
    val createdAt: LocalDateTime = LocalDateTime.now(),

    @Column(name = "updated_at", nullable = false)
    var updatedAt: LocalDateTime = LocalDateTime.now()
) {
    @PreUpdate
    fun onUpdate() {
        updatedAt = LocalDateTime.now()
    }
}
