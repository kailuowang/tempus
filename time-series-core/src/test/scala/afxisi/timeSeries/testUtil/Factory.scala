package afxisi.timeSeries.testUtil

import java.time._

import afxisi.timeSeries.time.Zone.EST
import afxisi.timeSeries.time.{Periodical, Zone}
import afxisi.timeSeries.{ListTimeSeries, PeriodicalSeries, TimeStamped}

trait Factory {

  def dateTimeOf(month: Int = 1,
                 day: Int = 1,
                 hour: Int = 0,
                 minute: Int = 0,
                 second: Int = 0,
                 year: Int = 1999,
                 zoneId: ZoneId = Zone.UTC.id): ZonedDateTime =
    ZonedDateTime.of(LocalDate.of(year, month, day),
                     LocalTime.of(hour, minute, second),
                     zoneId)

  def instantOf[Z <: Zone](month: Int = 1,
                           day: Int = 1,
                           hour: Int = 0,
                           minute: Int = 0,
                           second: Int = 0,
                           year: Int = 1999)(implicit Z: Z): Instant =
    ZonedDateTime
      .of(LocalDate.of(year, month, day), LocalTime.of(hour, minute, second), Z.id)
      .toInstant

  def instantEST(month: Int = 1,
                 day: Int = 1,
                 hour: Int = 0,
                 minute: Int = 0,
                 second: Int = 0,
                 year: Int = 1999): Instant =
    instantOf[Zone.EST](month, day, hour, minute, second, year)

  class PartialFromList[P <: Periodical, Z <: Zone] {
    def apply[A](l: (A, Instant)*)(implicit P: P,
                                   Z: Z): PeriodicalSeries[ListTimeSeries, P, Z, A] =
      PeriodicalSeries.from[P, Z](tsOf(l: _*))(_.last)
  }

  def tsOf[A](l: (A, Instant)*): ListTimeSeries[A] =
    ListTimeSeries.fromUnOrdered(l.toList.map(TimeStamped(_)))

  def fromList[P <: Periodical, Z <: Zone]: PartialFromList[P, Z] =
    new PartialFromList[P, Z]

  def fromListEST[P <: Periodical]: PartialFromList[P, EST] =
    new PartialFromList[P, EST]
}
