package tempus.timeSeries

import java.time.Instant

import cats.data._
import cats.implicits._
import cats.{Applicative, Eval, Foldable, Functor}
import io.estatico.newtype.ops._
import tempus.timeSeries.implicits._

import scala.annotation.tailrec

private [timeSeries] class ChainTimeSeriesFunctor extends Functor[ChainTimeSeries] {
  def mapTS[A, B](fa: ChainTimeSeries[A])(
    f: TimeStamped[A] => TimeStamped[B]): ChainTimeSeries[B] =
    fa.chain.map(f).coerce[ChainTimeSeries[B]]

  def map[A, B](fa: ChainTimeSeries[A])(f: A => B): ChainTimeSeries[B] =
    mapTS(fa)(_.map(f))
}

private [timeSeries] final class TimeSeriesForChainTimeSeries
  extends ChainTimeSeriesFunctor
    with TimeSeries[ChainTimeSeries] {

  override def empty[A]: ChainTimeSeries[A] = ChainTimeSeries(Chain.empty)

  def start[A](fa: ChainTimeSeries[A]): Option[Instant] = fa.chain.headOption.map(_.time)

  def last[A](fa: ChainTimeSeries[A]): Option[(A, Instant)] =
    fa.chain.toList.lastOption.coerce //todo: wait on cats release with Chain#lastOption

  def head[A](fa: ChainTimeSeries[A]): Option[(A, Instant)] =
    fa.chain.headOption.coerce

  def mapWithTime[A, B](fa: ChainTimeSeries[A])(
    f: (A, Instant) => B): ChainTimeSeries[B] =
    mapTS(fa)(tv => TimeStamped(f(tv.v, tv.time), tv.time))

  def traverseWithTime[A, B, G[_]](fa: ChainTimeSeries[A])(f: (A, Instant) => G[B])(
    implicit G: Applicative[G]): G[ChainTimeSeries[B]] =
    fa.chain
      .traverse {
        case TimeStamped(a, i) => f(a, i).map(TimeStamped(_, i))
      }
      .map(ChainTimeSeries(_))

  override def size[A](fa: ChainTimeSeries[A]): Long = fa.chain.length.toLong

  def traverse[G[_], A, B](fa: ChainTimeSeries[A])(f: A => G[B])(
    implicit G: Applicative[G]): G[ChainTimeSeries[B]] =
    fa.chain
      .traverse(tv => f(tv.v).map((value: B) => TimeStamped(value, tv.time)))
      .map(ChainTimeSeries(_))

  def foldLeft[A, B](fa: ChainTimeSeries[A], b: B)(f: (B, A) => B): B =
    fa.chain.foldLeft(b)((b, ts) => f(b, ts.v))

  def foldRight[A, B](fa: ChainTimeSeries[A], lb: Eval[B])(
    f: (A, Eval[B]) => Eval[B]): Eval[B] =
    Foldable[Chain].foldRight(fa.chain, lb)((ts, eb) => Eval.defer(f(ts.v, eb)))

  def groupByTime[A](fa: ChainTimeSeries[A])(
    f: Instant => Instant): ChainTimeSeries[NonEmptyList[A]] =
    ChainTimeSeries.fromUnOrderedList(
      fa.chain
        .groupBy(ts => f(ts.time))
        .map {
          case (time, timedValues) => TimeStamped(timedValues.map(_.v).toNonEmptyList, time)
        }.toList
    )


  def from[G[_]: Foldable, A](ga: G[(A, Instant)]): ChainTimeSeries[A] =
    ChainTimeSeries.fromUnOrderedList(ga.toList.coerce[List[TimeStamped[A]]])

  def fromOrdered[A](list: List[(A, Instant)]): ChainTimeSeries[A] =
    ChainTimeSeries(Chain.fromSeq(list).coerce[Chain[TimeStamped[A]]])

  def alignWithO[A, B, C](fa: ChainTimeSeries[A], fb: ChainTimeSeries[B])(
    fboth: (A, B) => Option[C],
    fAOnly: A => Option[C],
    fBOnly: B => Option[C]): ChainTimeSeries[C] = {
    @tailrec
    def go(va: Chain[TimeStamped[A]], vb: Chain[TimeStamped[B]], acc: Chain[TimeStamped[C]]): Chain[TimeStamped[C]] = (va, vb) match {
      case (ae, be) if ae.isEmpty && be.isEmpty => acc
      case (ae, _) if ae.isEmpty  => acc ++ vb.mapFilter(tb => fBOnly(tb.v).map(tb.as))
      case (_, be) if be.isEmpty  => acc ++ va.mapFilter(ta => fAOnly(ta.v).map(ta.as))
      case (ah ==: at, bh ==: _) if ah.time < bh.time =>
        go(at, vb, fAOnly(ah.v).fold(acc)(a => acc :+ ah.as(a)))

      case (ah ==: _, bh ==: bt) if ah.time > bh.time =>
        go(va, bt, fBOnly(bh.v).fold(acc)(b => acc :+ bh.as(b)))

      case (ah ==: at, bh ==: bt) if ah.time == bh.time =>
        go(at, bt, fboth(ah.v, bh.v).fold(acc)(v => acc :+ ah.as(v)))
    }

    ChainTimeSeries(go(fa.chain, fb.chain, Chain.empty))
  }

  def combineK[A](x: ChainTimeSeries[A], y: ChainTimeSeries[A]): ChainTimeSeries[A] =
    ChainTimeSeries.fromUnOrdered(x.chain ++ y.chain)

  override def filterByTime[A](fa: ChainTimeSeries[A])(
    f: Instant => Boolean): ChainTimeSeries[A] =
    ChainTimeSeries(fa.chain.filter(ta => f(ta.time)))

  override def after[A](fa: ChainTimeSeries[A], i: Instant): ChainTimeSeries[A] =
    ChainTimeSeries(fa.chain.filter(_.time.isAfter(i))) //todo: use Chain#dropWhile after Cats 2.0 release ChainTimeSeries(fa.chain.dropWhile(!_.time.isAfter(i)))

  override def before[A](fa: ChainTimeSeries[A], i: Instant): ChainTimeSeries[A] =
    ChainTimeSeries(fa.chain.filter(_.time.isBefore(i))) //todo: use Chain#takeWhile after Cats 2.0 release ChainTimeSeries(fa.chain.takeWhile(_.time.isBefore(i)))

  override def toListWithTime[A](fa: ChainTimeSeries[A]): List[(A, Instant)] =
    fa.chain.toList.coerce

}



object ==: {
  def unapply[T](c: Chain[T]): Option[(T, Chain[T])] =
    c.uncons

}