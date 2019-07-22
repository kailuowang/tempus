package tempus
package time

import java.time.temporal.IsoFields
import java.time.{Duration, Instant, ZoneId, ZonedDateTime, Period => JPeriod}
import cats.Show

import scala.annotation.{implicitNotFound}

sealed trait Periodical extends Product with Serializable {
  self =>
  private[time] def periodRawOf[Z <: Zone](time: Instant)(
      implicit Z: Z): (Instant, Duration) = {
    val (findStart, jp) = startDateAndJPeriod(time, Z.id)
    val start = findStart(ZonedDateTime.ofInstant(time, Z.id))
      .withHour(0)
      .withMinute(0)
      .withSecond(0)
      .withNano(0)
    val end = start.plus(jp).minusNanos(1)
    (start.toInstant, Duration.between(start, end))
  }

  def lengthInDays: Double

  protected def startDateAndJPeriod(
      time: Instant,
      zoneId: ZoneId): (ZonedDateTime => ZonedDateTime, JPeriod)
}

object Periodical {

  implicit case object Monthly extends Periodical {
    val lengthInDays = 30.416667d

    protected def startDateAndJPeriod(
        time: Instant,
        zoneId: ZoneId): (ZonedDateTime => ZonedDateTime, JPeriod) =
      (_.withDayOfMonth(1), JPeriod.ofMonths(1))
  }

  implicit case object Quarterly extends Periodical {
    val lengthInDays = 91d

    protected def startDateAndJPeriod(
        time: Instant,
        zoneId: ZoneId): (ZonedDateTime => ZonedDateTime, JPeriod) =
      ((d: ZonedDateTime) =>
         d.withDayOfMonth(1).withMonth((d.get(IsoFields.QUARTER_OF_YEAR) - 1) * 3 + 1),
       JPeriod.ofMonths(3))
  }

  implicit case object Annually extends Periodical {
    val lengthInDays = 365d

    protected def startDateAndJPeriod(
        time: Instant,
        zoneId: ZoneId): (ZonedDateTime => ZonedDateTime, JPeriod) =
      (_.withDayOfYear(1), JPeriod.ofYears(1))
  }

  implicit case object Daily extends Periodical {
    val lengthInDays = 1d

    protected def startDateAndJPeriod(
        time: Instant,
        zoneId: ZoneId): (ZonedDateTime => ZonedDateTime, JPeriod) =
      (identity, JPeriod.ofDays(1))
  }

  type Monthly = Monthly.type
  type Quarterly = Quarterly.type
  type Annually = Annually.type
  type Daily = Daily.type

  implicit def periodicalShow: Show[Periodical] = new Show[Periodical] {
    def show(p: Periodical): String = p match {
      case Annually  => "Annually"
      case Monthly   => "Monthly"
      case Quarterly => "Quarterly"
      case Daily     => "Daily"
    }
  }

  @implicitNotFound("${P1} is not larger than ${P2}")
  final class Include[P1 <: Periodical, P2 <: Periodical] private ()

  object Include {
    implicit def selfInclude[P <: Periodical]: P Include P = new Include[P, P]

    implicit val monthlyDaily: Monthly Include Daily = new Include[Monthly, Daily]
    implicit val quarterlyMonthly: Quarterly Include Monthly =
      new Include[Quarterly, Monthly]
    implicit val quarterlyDaily: Quarterly Include Daily = new Include[Quarterly, Daily]
    implicit val annuallyQuarterly: Annually Include Quarterly =
      new Include[Annually, Quarterly]
    implicit val annuallyMonthly: Annually Include Monthly =
      new Include[Annually, Monthly]
    implicit val annuallyDaily: Annually Include Daily = new Include[Annually, Daily]

  }

}
