package tempus
package time

import java.time.{Duration, Instant, LocalDate}

import tempus.time.Periodical.Include
import cats.{PartialOrder, Show}
import implicits._
import cats.implicits._

import scala.annotation.tailrec

sealed trait PeriodBase {
  def start: Instant

  def duration: Duration

  lazy val end: Instant = start.plus(duration)

  /**
    * Period end but no later than now.
    *
    * @return
    */
  def endOrNow: Instant = {
    val now = Instant.now()
    if (end isAfter now) now else end
  }

  //todo: clean up, this one can be replaced with start.date using TimeSyntax
  def startDate[Z <: Zone](implicit Z: Z): LocalDate =
    start.atZone(Z.id).toLocalDate

  def portionOf[P <: Periodical, Z <: Zone](implicit P: P, Z: Z): Double =
    duration.getSeconds.toDouble / Period.of[P, Z](start).duration.getSeconds.toDouble

}

case class FreePeriod(start: Instant, duration: Duration) extends PeriodBase

object FreePeriod {
  def apply(start: Instant, end: Instant): FreePeriod =
    new FreePeriod(start, Duration.between(start, end))
}

object PeriodBase {
  implicit val showPeriodBase: Show[PeriodBase] =
    Show.show(p => s"Period(${p.start}, ${p.end})")

  implicit def partialOrderPeriod: PartialOrder[FreePeriod] =
    new PartialOrder[FreePeriod] {
      def partialCompare(x: FreePeriod, y: FreePeriod): Double = {
        if (x.end.isBefore(y.start))
          -1.0
        else if (y.end.isBefore(x.start))
          1.0
        else if (x.start === y.start && x.duration === y.duration)
          0
        else
          Double.NaN
      }
    }

}

case class Period[P <: Periodical, Z <: Zone] private (
    val start: Instant,
    val duration: Duration)(implicit P: P, Z: Z)
    extends PeriodBase {
  def nPeriodicalAway[P2 <: Periodical](n: Int)(implicit P2: P2,
                                                ev: P2 Include P): Period[P, Z] = {
    val numOfMyP = Math.round(P2.lengthInDays / P.lengthInDays) * n

    @tailrec
    def go(steps: Int, p: Period[P, Z]): Period[P, Z] = {
      if (steps == 0) p
      else if (steps > 0)
        go(steps - 1, p.next)
      else
        go(steps + 1, p.previous)
    }

    go(numOfMyP.toInt, this)
  }

  def next: Period[P, Z] = Period.of(end.plusNanos(1))

  def previous: Period[P, Z] = Period.of(start.minusNanos(1))

  def startDate: LocalDate = startDate[Z]

  def portionOf[P2 <: Periodical](implicit P2: P2): Double = portionOf[P2, Z]

}

object Period {
  def of[P <: Periodical, Z <: Zone](time: Instant)(implicit P: P, Z: Z): Period[P, Z] = {
    val (s, d) = P.periodRawOf[Z](time)
    new Period(s, d)
  }

}
