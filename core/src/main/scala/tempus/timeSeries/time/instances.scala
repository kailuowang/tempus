package tempus.timeSeries.time

import java.time.{Duration, Instant, LocalDate}

import cats.{Order, Show}
import cats.instances.long._

trait InstantInstances {
  implicit val instantOrder: Order[Instant] = Order.fromOrdering
  implicit val instantShow: Show[Instant] = Show.fromToString
}

trait DurationInstances {
  implicit val durationOrder: Order[Duration] = Order.fromOrdering

}

trait LocalDateInstances {
  implicit val localDateShow: Show[LocalDate] = Show.fromToString
  implicit val localDateOrder: Order[LocalDate] = Order.by(_.toEpochDay)
}
