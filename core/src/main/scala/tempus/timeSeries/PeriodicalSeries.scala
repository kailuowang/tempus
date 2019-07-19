package tempus
package timeSeries

import java.time.{Instant, LocalDate}

import tempus.timeSeries.PeriodicalSeries.condensePartial
import tempus.timeSeries.time.Periodical.{Daily, Include}
import cats.{Applicative, Eval, Foldable, Functor, MonoidK, Show, Traverse}
import cats.data.{Ior, NonEmptyList, State}
import time.{Period, Periodical, Zone}
import implicits._
import cats.implicits._
import cats.implicits._
import implicits._
import cats.FunctorFilter
import scala.collection.mutable.ListBuffer

case class PeriodicalSeries[TS[_], P <: Periodical, Z <: Zone, A] private[timeSeries] (
    ts: TS[A])(implicit P: P, Z: Z, TS: TimeSeries[TS]) {

  type PS[T] = PeriodicalSeries[TS, P, Z, T]

  /**
    * Reduce to a longer periodical
    *
    * {{{
    * scala> import cats.implicits._
    * scala> import tempus.timeSeries._, time._, Periodical._
    * scala> import tempus.timeSeries.testUtil._
    *
    * scala> val ts = TimeSeries[ListTimeSeries].from(List(
    *      | (1, dateTimeOf(1, 1).toInstant),
    *      | (2, dateTimeOf(2, 1).toInstant),
    *      | (5, dateTimeOf(4, 1).toInstant)))
    * scala> val dps = PeriodicalSeries.from[Monthly, Zone.UTC](ts)(_.head)
    *
    * scala> dps.condense[Quarterly](_.fold).show
    * res0: String =
    * Quarterly Time Series
    * 1999-01-01T00:00:00Z - 3
    * 1999-04-01T00:00:00Z - 5
    * at UTC
    * }}}
    */
  def condense[P2 <: Periodical] = new condensePartial[TS, A, P, P2, Z](this)

  def start: Option[Instant] = ts.start

  def head: Option[(A, Instant)] = ts.head

  def size: Long = ts.size

  def map[B](f: A => B): PS[B] =
    copy(ts = ts.map(f))

  def filter(f: A => Boolean): PS[A] =
    copy(ts.filter(f))

  def mapWithPeriod[B](f: (A, Period[P, Z]) => B): PS[B] =
    copy(ts = ts.mapWithTime((a, i) => f(a, Period.of(i))))

  def traverse[F[_]: Applicative, B](f: A => F[B]): F[PS[B]] =
    ts.traverse(f).map(nt => copy(ts = nt))

  def traverseWithTime[F[_]: Applicative, B](f: (A, Instant) => F[B]): F[PS[B]] =
    ts.traverseWithTime(f).map(nt => copy(ts = nt))

  def traverseWithPeriod[F[_]: Applicative, B](f: (A, Period[P, Z]) => F[B]): F[PS[B]] =
    traverseWithTime((a, i) => f(a, Period.of(i)))

  def merge[B](that: PS[B]): PS[A Ior B] =
    copy(ts.merge(that.ts))

  def after(instant: Instant): PS[A] =
    copy(ts.after(instant))

  def before(instant: Instant): PS[A] =
    copy(ts.before(Period.of[P, Z](instant).start))

  def outerJoin[B](that: PS[B]): PS[A Ior B] = merge(that)

  def mapFilter[B](f: A => Option[B]): PeriodicalSeries[TS, P, Z, B] =
    PeriodicalSeries(TS.mapFilter(ts)(f))

  def innerJoin[B](that: PS[B]): PS[(A, B)] =
    PeriodicalSeries(ts.innerJoin(that.ts))

  def merge3[B, C](pb: PS[B], pc: PS[C]): PS[(Option[A], Option[B], Option[C])] =
    copy(ts.merge3(pb.ts, pc.ts))

  def merge4[B, C, D](pb: PS[B],
                      pc: PS[C],
                      pd: PS[D]): PS[(Option[A], Option[B], Option[C], Option[D])] =
    copy(ts.merge4(pb.ts, pc.ts, pd.ts))

  def toList: List[A] = ts.toList

  def last: Option[(A, Period[P, Z])] = ts.last.map { case (a, i) => (a, Period.of(i)) }

  def lastValue: Option[A] = ts.lastValue

  def findByTime(instant: Instant): Option[A] =
    ts.findByTime(Period.of[P, Z](instant).start)

  /**
    * fill the gaps between
    */
  def fill(until: Option[Instant] = None): PS[A] = {
    (head, until.orElse(ts.last.map(_._2)))
      .mapN { (sp, e) =>
        val (headValue, start) = sp
        PeriodicalSeries
          .fill[TS, P, Z](start, e, ())
          .merge(this)
          .traverse { i =>
            State { (lastValue: A) =>
              val newValue = i.right.getOrElse(lastValue)
              (newValue, newValue)
            }
          }
          .runA(headValue)
          .value
      }
      .getOrElse(PeriodicalSeries.empty)
  }

  /**
    *
    * @param n number of WPs in the window
    * @tparam WP the Periodical to measure the window size.
    * @return
    */
  def movingWindow[WP <: Periodical](n: Int)(implicit ev: WP Include P,
                                             P2: WP): PS[PS[A]] = {
    assert(n > 0, "window size must be larger than zero")
    var lb = ListBuffer.empty[(A, Instant)]
    mapWithPeriod {
      case (a, p) =>
        lb = lb.dropWhile {
          case (_, lt) => !lt.isAfter(p.nPeriodicalAway[WP](-n).end)
        }
        lb.append((a, p.start))
        PeriodicalSeries(TS.from(lb.toList))
    }
  }
}

object PeriodicalSeries extends PeriodicalSeriesInstances0 {

  def empty[TS[_], P <: Periodical, Z <: Zone, A](implicit TS: TimeSeries[TS],
                                                  P: P,
                                                  Z: Z): PeriodicalSeries[TS, P, Z, A] =
    PeriodicalSeries(TS.empty[A])

  def fill[TS[_], P <: Periodical, Z <: Zone]: fillPartial[TS, P, Z] =
    new fillPartial[TS, P, Z]()

  def fillWith[TS[_], P <: Periodical, Z <: Zone]: fillWithPartial[TS, P, Z] =
    new fillWithPartial[TS, P, Z]()

  def from[P <: Periodical, Z <: Zone] = new fromPartial[P, Z]

  private[timeSeries] final class fromPartial[P <: Periodical, Z <: Zone](
      val dummy: Boolean = true)
      extends AnyVal {
    def apply[TS[_], A, B](fa: TS[A])(f: NonEmptyList[A] => B)(
        implicit P: P,
        Z: Z,
        TS: TimeSeries[TS]): PeriodicalSeries[TS, P, Z, B] =
      PeriodicalSeries(fa.groupByTime(Period.of[P, Z](_).start).map(f))
  }

  /**
    * Create a PeriodicalSeries directly from a TimeSeries without checking its data,
    * assuming it's already the correct data representation.
    * Mainly for performance reason.
    */
  def fromUnsafe[P <: Periodical, Z <: Zone] = new fromUnsafePartial[P, Z]

  private[timeSeries] final class fromUnsafePartial[P <: Periodical, Z <: Zone](
      val dummy: Boolean = true)
      extends AnyVal {
    def apply[TS[_], A](ts: TS[A])(implicit P: P,
                                   Z: Z,
                                   TS: TimeSeries[TS]): PeriodicalSeries[TS, P, Z, A] =
      PeriodicalSeries(ts)
  }

  /**
    * The assumption is that ga has no duplicated date
    */
  def fromDailyData[TS[_], Z <: Zone]: fromDailyDataPartial[TS, Z] =
    new fromDailyDataPartial[TS, Z]()

  private[timeSeries] final class fromDailyDataPartial[TS[_], Z <: Zone](
      val dummy: Boolean = true)
      extends AnyVal {
    def apply[A, F[_]: Foldable: Functor](data: F[(A, LocalDate)])(
        implicit Z: Z,
        TS: TimeSeries[TS]): PeriodicalSeries[TS, Daily, Z, A] =
      PeriodicalSeries[TS, Daily, Z, A](TS.from(data.map(_.map(_.start))))
  }

  private[timeSeries] final class condensePartial[TS[_], A, P <: Periodical,
  P2 <: Periodical, Z <: Zone](val po: PeriodicalSeries[TS, P, Z, A])
      extends AnyVal {
    def apply[B](f: NonEmptyList[A] => B)(
        implicit P2: P2,
        ev: P2 Include P,
        Z: Z,
        TS: TimeSeries[TS]): PeriodicalSeries[TS, P2, Z, B] =
      PeriodicalSeries.from[P2, Z](po.ts)(f)
  }

  private[timeSeries] final class fillPartial[TS[_], P <: Periodical, Z <: Zone](
      val dummy: Boolean = true)
      extends AnyVal {
    def apply[A](start: Instant, end: Instant, a: A)(
        implicit P: P,
        Z: Z,
        TS: TimeSeries[TS]): PeriodicalSeries[TS, P, Z, A] =
      fillWith[TS, P, Z](start, end)(_ => a)
  }

  private[timeSeries] final class fillWithPartial[TS[_], P <: Periodical, Z <: Zone](
      val dummy: Boolean = true)
      extends AnyVal {
    def apply[A](start: Instant, end: Instant)(f: Period[P, Z] => A)(
        implicit P: P,
        Z: Z,
        TS: TimeSeries[TS]): PeriodicalSeries[TS, P, Z, A] = {
      assert(!end.isBefore(start), s"try to fill from $start to $end")
      val increment: State[NonEmptyList[(A, Period[P, Z])], Unit] = State.modify { ls =>
        val p = Period.of[P, Z](ls.head._2.end.plusNanos(1))
        (f(p), p) :: ls
      }

      val initP = Period.of[P, Z](start)
      val initS = NonEmptyList.one((f(initP), initP))
      val ls = increment
        .whileM_(State.inspect(ls => ls.head._2.end.isBefore(end)))
        .runS(initS)
        .value

      PeriodicalSeries(TS.from(ls.reverse.map { case (a, p) => (a, p.start) }))
    }

  }

}

sealed abstract class PeriodicalSeriesInstances0 extends PeriodicalSeriesInstances1 {
  implicit def showForPeriodSeries[TS[_], P <: Periodical, Z <: Zone, A](
      implicit A: Show[A],
      showTS: Show[TS[A]],
      p: P,
      Z: Z,
      showP: Show[Periodical]): Show[PeriodicalSeries[TS, P, Z, A]] =
    new Show[PeriodicalSeries[TS, P, Z, A]] {
      def show(t: PeriodicalSeries[TS, P, Z, A]): String =
        show"$p ${t.ts}" + "at " + Z.id.toString
    }

  implicit def monoidKForPeriodSeries[TS[_], P <: Periodical, Z <: Zone](
      implicit TS: TimeSeries[TS],
      P: P,
      Z: Z): MonoidK[PeriodicalSeries[TS, P, Z, ?]] =
    new MonoidK[PeriodicalSeries[TS, P, Z, ?]] {
      def empty[A]: PeriodicalSeries[TS, P, Z, A] = PeriodicalSeries.empty

      def combineK[A](fa: PeriodicalSeries[TS, P, Z, A],
                      fb: PeriodicalSeries[TS, P, Z, A]): PeriodicalSeries[TS, P, Z, A] =
        PeriodicalSeries(fa.ts <+> fb.ts)
    }

  implicit def functorEmptyPeriodSeries[TS[_], P <: Periodical, Z <: Zone](
      implicit TS: TimeSeries[TS],
      P: P,
      Z: Z): FunctorFilter[PeriodicalSeries[TS, P, Z, ?]] =
    new FunctorFilter[PeriodicalSeries[TS, P, Z, ?]] {
      val functor: Functor[PeriodicalSeries[TS, P, Z, ?]] =
        new Functor[PeriodicalSeries[TS, P, Z, ?]] {
          def map[A, B](fa: PeriodicalSeries[TS, P, Z, A])(
              f: A => B): PeriodicalSeries[TS, P, Z, B] = fa.map(f)
        }

      def mapFilter[A, B](fa: PeriodicalSeries[TS, P, Z, A])(
          f: A => Option[B]): PeriodicalSeries[TS, P, Z, B] =
        fa.mapFilter(f)
    }

  implicit def functorPeriodSeries[TS[_]: TimeSeries, P <: Periodical, Z <: Zone](
      implicit P: P,
      Z: Z): Functor[PeriodicalSeries[TS, P, Z, ?]] =
    functorEmptyPeriodSeries[TS, P, Z].functor
}

sealed abstract class PeriodicalSeriesInstances1 {

  implicit def traversePeriodSeries[TS[_], P <: Periodical, Z <: Zone](
      implicit TS: TimeSeries[TS],
      P: P,
      Z: Z): Traverse[PeriodicalSeries[TS, P, Z, ?]] =
    new Traverse[PeriodicalSeries[TS, P, Z, ?]] {
      def traverse[F[_]: Applicative, A, B](fa: PeriodicalSeries[TS, P, Z, A])(
          f: A => F[B]): F[PeriodicalSeries[TS, P, Z, B]] =
        fa.traverse(f)

      def foldLeft[A, B](fa: PeriodicalSeries[TS, P, Z, A], b: B)(f: (B, A) => B): B =
        fa.ts.foldLeft(b)(f)

      def foldRight[A, B](fa: PeriodicalSeries[TS, P, Z, A], lb: Eval[B])(
          f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.ts.foldRight(lb)(f)
    }
}
