package tempus.timeSeries.syntax

import java.time.{Instant, LocalDate}
import java.time.temporal.ChronoUnit.NANOS

import tempus.time.Zone

import scala.concurrent.duration.Duration
import scala.concurrent.duration._

trait TimeSyntax {

  implicit class durationOps(d: Duration) {
    def ago: Instant =
      Instant.now.minus(d.toNanos, NANOS)

    def after: Instant =
      Instant.now.plus(d.toNanos, NANOS)
  }

  implicit class numberDurationOps(i: Int) {
    def years: Duration = (365 * i).days
  }

  implicit class instantOps(self: Instant) {
    def date(implicit Z: Zone): LocalDate = self.atZone(Z.id).toLocalDate
  }

  implicit class localDateOps(self: LocalDate) {
    def start(implicit Z: Zone): Instant = self.atStartOfDay(Z.id).toInstant
  }
}
