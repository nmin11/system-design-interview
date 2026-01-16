package proximity.lbs

import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication

@SpringBootApplication
class LocationBasedServiceApplication

fun main(args: Array<String>) {
	runApplication<LocationBasedServiceApplication>(*args)
}
