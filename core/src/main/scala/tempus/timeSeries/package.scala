package tempus

import java.time.Instant

import cats.{Eq, Functor, Show}
import cats.kernel.Order
import io.estatico.newtype.macros.newtype
import io.estatico.newtype.ops._
import cats.implicits._
import timeSeries.implicits._

package object timeSeries {
  @newtype case class TimeStamped[A](tuple: (A, Instant)) {
    def v: A = tuple._1
    def time: Instant = tuple._2
  }

  object TimeStamped extends TimeStampedInstances {
    def apply[A](a: A, i: Instant): TimeStamped[A] = TimeStamped((a, i))

    def unapply[A](arg: TimeStamped[A]): Option[(A, Instant)] = Some(arg.coerce)
  }

  private[timeSeries] abstract class TimeStampedInstances
      extends TimeStampedInstancesLower {
    implicit val tsFunctor: Functor[TimeStamped] = new Functor[TimeStamped] {
      def map[A, B](ta: TimeStamped[A])(f: A => B): TimeStamped[B] =
        TimeStamped(f(ta.v), ta.time)
    }

    implicit def tsOrder[A: Eq]: Order[TimeStamped[A]] = new TimeStampOrder[A] {
      override def eqv(x: TimeStamped[A], y: TimeStamped[A]): Boolean =
        x.v === y.v && x.time === y.time
    }

    implicit def tsShow[A: Show]: Show[TimeStamped[A]] = Show.show[TimeStamped[A]] {
      tsa =>
        tsa.time.toString + " - " + tsa.v.show
    }

    implicit def tsOrdering[A: Eq]: Ordering[TimeStamped[A]] = tsOrder[A].toOrdering
  }

  private[timeSeries] abstract class TimeStampedInstancesLower {
    implicit def tsOrderNoEq[A]: Order[TimeStamped[A]] = new TimeStampOrder[A]
  }

  private[timeSeries] class TimeStampOrder[A] extends Order[TimeStamped[A]] {
    def compare(x: TimeStamped[A], y: TimeStamped[A]): Int = x.time.compare(y.time)
  }


  @newtype
  case class ListTimeSeries[A](list: List[TimeStamped[A]])

  object ListTimeSeries {

    def fromUnOrdered[A](v: List[TimeStamped[A]]) = ListTimeSeries(v.sorted)

    implicit def showForListTimeSeries[A: Show]: Show[ListTimeSeries[A]] =
      Show.show(
        _.list
          .map(_.show)
          .mkString(
            "Time Series\n",
            "\n",
            "\n"
          ))

    implicit val timeSeriesForListTimeSeries: TimeSeries[ListTimeSeries] =
      new TimeSeriesForListTimeSeries




  }

}
