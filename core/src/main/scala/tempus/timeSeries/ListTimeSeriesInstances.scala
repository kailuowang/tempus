package tempus
package timeSeries

import java.time.Instant

import cats.data.NonEmptyList
import cats.{Applicative, Eval, Foldable, Functor}
import cats.implicits._
import implicits._

import scala.annotation.tailrec
import io.estatico.newtype.ops._

private [timeSeries] class ListTimeSeriesFunctor extends Functor[ListTimeSeries] {
  def mapTS[A, B](fa: ListTimeSeries[A])(
    f: TimeStamped[A] => TimeStamped[B]): ListTimeSeries[B] =
    fa.list.map(f).coerce[ListTimeSeries[B]]

  def map[A, B](fa: ListTimeSeries[A])(f: A => B): ListTimeSeries[B] =
    mapTS(fa)(_.map(f))
}

private [timeSeries] final class TimeSeriesForListTimeSeries
  extends ListTimeSeriesFunctor
    with TimeSeries[ListTimeSeries] {

  override def empty[A]: ListTimeSeries[A] = ListTimeSeries(Nil)

  def start[A](fa: ListTimeSeries[A]): Option[Instant] = fa.list.headOption.map(_.time)

  def last[A](fa: ListTimeSeries[A]): Option[(A, Instant)] =
    fa.list.lastOption.coerce

  def head[A](fa: ListTimeSeries[A]): Option[(A, Instant)] =
    fa.list.headOption.coerce

  def mapWithTime[A, B](fa: ListTimeSeries[A])(
    f: (A, Instant) => B): ListTimeSeries[B] =
    mapTS(fa)(tv => TimeStamped(f(tv.v, tv.time), tv.time))

  def traverseWithTime[A, B, G[_]](fa: ListTimeSeries[A])(f: (A, Instant) => G[B])(
    implicit G: Applicative[G]): G[ListTimeSeries[B]] =
    fa.list
      .traverse {
        case TimeStamped(a, i) => f(a, i).map(TimeStamped(_, i))
      }
      .map(ListTimeSeries(_))

  override def size[A](fa: ListTimeSeries[A]): Long = fa.list.length.toLong

  def traverse[G[_], A, B](fa: ListTimeSeries[A])(f: A => G[B])(
    implicit G: Applicative[G]): G[ListTimeSeries[B]] =
    fa.list
      .traverse(tv => f(tv.v).map((value: B) => TimeStamped(value, tv.time)))
      .map(ListTimeSeries(_))

  def foldLeft[A, B](fa: ListTimeSeries[A], b: B)(f: (B, A) => B): B =
    fa.list.foldLeft(b)((b, ts) => f(b, ts.v))

  def foldRight[A, B](fa: ListTimeSeries[A], lb: Eval[B])(
    f: (A, Eval[B]) => Eval[B]): Eval[B] =
    Foldable[List].foldRight(fa.list, lb)((ts, eb) => Eval.defer(f(ts.v, eb)))

  def groupByTime[A](fa: ListTimeSeries[A])(
    f: Instant => Instant): ListTimeSeries[NonEmptyList[A]] =
    ListTimeSeries.fromUnOrdered(
      fa.list
        .groupByNel(ts => f(ts.time))
        .map {
          case (time, timedValues) => TimeStamped(timedValues.map(_.v), time)
        }
        .toList)

  def from[G[_]: Foldable, A](ga: G[(A, Instant)]): ListTimeSeries[A] =
    ListTimeSeries.fromUnOrdered(ga.toList.coerce[List[TimeStamped[A]]])

  def fromOrdered[A](list: List[(A, Instant)]): ListTimeSeries[A] =
    ListTimeSeries(list.coerce[List[TimeStamped[A]]])

  def alignWithO[A, B, C](fa: ListTimeSeries[A], fb: ListTimeSeries[B])(
    fboth: (A, B) => Option[C],
    fAOnly: A => Option[C],
    fBOnly: B => Option[C]): ListTimeSeries[C] = {
    val builder = List.newBuilder[TimeStamped[C]]

    @tailrec
    def go(va: List[TimeStamped[A]], vb: List[TimeStamped[B]]): Unit = (va, vb) match {
      case (Nil, Nil) => ()
      case (Nil, _)   => builder ++= vb.flatMap(tb => fBOnly(tb.v).map(tb.as)); ()
      case (_, Nil)   => builder ++= va.flatMap(ta => fAOnly(ta.v).map(ta.as)); ()
      case (ah :: at, bh :: _) if ah.time < bh.time =>
        fAOnly(ah.v).foreach(a => builder += ah.as(a))
        go(at, vb)

      case (ah :: _, bh :: bt) if ah.time > bh.time =>
        fBOnly(bh.v).foreach(b => builder += bh.as(b))
        go(va, bt)

      case (ah :: at, bh :: bt) if ah.time == bh.time =>
        fboth(ah.v, bh.v).foreach(v => builder += ah.as(v))
        go(at, bt)
    }

    go(fa.list, fb.list)
    ListTimeSeries(builder.result)
  }

  def combineK[A](x: ListTimeSeries[A], y: ListTimeSeries[A]): ListTimeSeries[A] =
    ListTimeSeries.fromUnOrdered(x.list ++ y.list)

  override def filterByTime[A](fa: ListTimeSeries[A])(
    f: Instant => Boolean): ListTimeSeries[A] =
    ListTimeSeries(fa.list.filter(ta => f(ta.time)))

  override def after[A](fa: ListTimeSeries[A], i: Instant): ListTimeSeries[A] =
    ListTimeSeries(fa.list.dropWhile(!_.time.isAfter(i)))

  override def before[A](fa: ListTimeSeries[A], i: Instant): ListTimeSeries[A] =
    ListTimeSeries(fa.list.takeWhile(_.time.isBefore(i)))

  override def toListWithTime[A](fa: ListTimeSeries[A]): List[(A, Instant)] =
    fa.list.coerce

  override def toList[A](fa: ListTimeSeries[A]): List[A] = fa.list.map(_.v)
}

