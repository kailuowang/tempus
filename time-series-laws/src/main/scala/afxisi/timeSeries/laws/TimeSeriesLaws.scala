package afxisi.timeSeries.laws

import java.time.Instant

import afxisi.timeSeries.TimeSeries
import cats.laws.{IsEq, TraverseLaws}
import afxisi.timeSeries.implicits._
import cats.data.Ior.Both
import cats.{Applicative, Foldable}
import cats.data.{Ior, NonEmptyList}
import cats.laws._
import cats.implicits._

trait TimeSeriesLaws[F[_]]
    extends TraverseLaws[F]
    with MonoidKLaws[F]
    with FunctorFilterLaws[F] {
  implicit def F: TimeSeries[F]

  def mapWithTimeConsistentWithMap[A, B](fa: F[A], f: A => B): IsEq[F[B]] =
    fa.map(f) <-> fa.mapWithTime((a, _) => f(a))

  def traverseWithTimeConsistentWithTraverse[A, B, G[_]: Applicative](
      fa: F[A],
      f: A => G[B]): IsEq[G[F[B]]] =
    fa.traverse(f) <-> fa.traverseWithTime((a, _) => f(a))

  def groupByTimeIdentity[A](fa: F[A]): IsEq[F[NonEmptyList[A]]] =
    fa.groupByTime(identity) <-> fa.map(NonEmptyList.one)

  def groupByTimeSequence[A](fa: F[A], periodInSecondsSeed: Long): IsEq[List[A]] = {
    val periodInSeconds = Math.max(1L, periodInSecondsSeed)
    fa.groupByTime(i =>
        Instant.ofEpochSecond(i.getEpochSecond / periodInSeconds * periodInSeconds))
      .map(_.toList)
      .toList
      .flatten <-> fa.toList
  }

  def fromConsistency[A, G[_]: Foldable](ga: G[(A, Instant)]): IsEq[List[A]] =
    F.from(ga).toList <-> ga.toList.sortBy(_._2).map(_._1)

  def lastConsistency[A, G[_]: Foldable](
      ga: G[(A, Instant)]): IsEq[Option[(A, Instant)]] =
    F.from(ga).last <-> ga.toList.sortBy(_._2).lastOption

  def innerJoinWithDuplication[A, B](fa: F[A], f: (A, A) => B): IsEq[F[B]] =
    fa.innerJoinWith(fa)(f) <-> fa.map(a => f(a, a))

  def innerJoinWithLeftOnly[A, B](fa: F[A], f: (A, A) => B): IsEq[F[B]] =
    fa.innerJoinWith(F.empty)(f) <-> F.empty

  def innerJoinWithRightOnly[A, B](fa: F[A], f: (A, A) => B): IsEq[F[B]] =
    F.empty[A].innerJoinWith(fa)(f) <-> F.empty

  def outerJoinConsistent[A, B](fa: F[A], fb: F[B]): IsEq[F[Ior[A, B]]] = {
    val aMap = fa.mapWithTime((a, i) => (i, a)).toList.toMap
    val bMap = fb.mapWithTime((b, i) => (i, b)).toList.toMap
    val expected = F.from((aMap.map {
      case (k, v) =>
        (k, bMap.get(k).fold(v.leftIor[B])(Both(v, _)))
    } ++ bMap.map {
      case (k, v) =>
        (k, aMap.get(k).fold(v.rightIor[A])(Both(_, v)))
    }).toList.map(_.swap))
    fa.outerJoin(fb) <-> expected
  }

}

object TimeSeriesLaws {
  def apply[F[_]](implicit ev: TimeSeries[F]): TimeSeriesLaws[F] =
    new TimeSeriesLaws[F] {
      def F: TimeSeries[F] = ev
    }
}
