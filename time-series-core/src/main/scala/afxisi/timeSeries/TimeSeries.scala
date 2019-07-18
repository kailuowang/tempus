package afxisi
package timeSeries

import java.time.Instant

import cats.data.Ior.Both
import cats.data.{Ior, NonEmptyList}
import cats.{Applicative, Foldable, Functor, MonoidK, Traverse}
import simulacrum.typeclass
import cats.instances.list._
import cats.FunctorFilter
import cats.syntax.ior._

@typeclass
trait TimeSeries[F[_]] extends Traverse[F] with FunctorFilter[F] with MonoidK[F] {
  self =>
  override val functor: Functor[F] = self

  def mapWithTime[A, B](fa: F[A])(f: (A, Instant) => B): F[B]

  def traverseWithTime[A, B, G[_]: Applicative](fa: F[A])(
      f: (A, Instant) => G[B]): G[F[B]]

  def groupByTime[A](fa: F[A])(f: Instant => Instant): F[NonEmptyList[A]]

  def from[G[_]: Foldable, A](ga: G[(A, Instant)]): F[A]

  /**
    * Create from list already sorted chronically
    * If the input list is of incorrectness, the resulting time series F[A] will be incorrect too
    */
  def fromOrdered[A](list: List[(A, Instant)]): F[A]

  def last[A](fa: F[A]): Option[(A, Instant)]

  def start[A](fa: F[A]): Option[Instant]

  def head[A](fa: F[A]): Option[(A, Instant)]

  def toListWithTime[A](fa: F[A]): List[(A, Instant)] = toList(mapWithTime(fa)((_, _)))

  def after[A](fa: F[A], i: Instant): F[A] = filterByTime(fa)(time => time.isAfter(i))

  def before[A](fa: F[A], i: Instant): F[A] = filterByTime(fa)(time => time.isBefore(i))

  def empty[A]: F[A] = from(List.empty[(A, Instant)])

  def alignWithO[A, B, C](fa: F[A], fb: F[B])(fBoth: (A, B) => Option[C],
                                              fAOnly: A => Option[C],
                                              fBOnly: B => Option[C]): F[C]

  override def mapFilter[A, B](fa: F[A])(f: A => Option[B]): F[B] = {
    alignWithO(fa, empty[A])(
      (_, _) => None,
      f,
      _ => None
    )
  }

  def filterByTime[A](fa: F[A])(f: Instant => Boolean): F[A] =
    flattenOption(
      mapWithTime(fa)((a, i) => if (f(i)) Some(a) else None)
    )

  def lastValue[A](fa: F[A]): Option[A] = last(fa).map(_._1)

  def findByTime[A](fa: F[A], i: Instant): Option[A] = lastValue(filterByTime(fa)(_ == i))

  def outerJoin[A, B](fa: F[A], fb: F[B]): F[A Ior B] = alignWithO(fa, fb)(
    (a, b) => Some(Both(a, b)),
    a => Some(a.leftIor),
    b => Some(b.rightIor)
  )

  def innerJoin[A, B](fa: F[A], fb: F[B]): F[(A, B)] = innerJoinWith(fa, fb)((_, _))

  def innerJoinWith[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    alignWithO(fa, fb)(
      (a, b) => Some(f(a, b)),
      _ => None,
      _ => None
    )

  def merge[A, B](fa: F[A], fb: F[B]): F[A Ior B] = outerJoin(fa, fb)

  def alignWith[A, B, C](fa: F[A], fb: F[B])(f: Ior[A, B] => C): F[C] =
    alignWithO(fa, fb)(
      (a, b) => Some(f(Both(a, b))),
      a => Some(f(a.leftIor)),
      b => Some(f(b.rightIor))
    )

  def merge3[A, B, C](fa: F[A],
                      fb: F[B],
                      fc: F[C]): F[(Option[A], Option[B], Option[C])] =
    map(merge(merge(fa, fb), fc)) { i =>
      (i.left.flatMap(_.left), i.left.flatMap(_.right), i.right)
    }

  def merge4[A, B, C, D](fa: F[A],
                         fb: F[B],
                         fc: F[C],
                         fd: F[D]): F[(Option[A], Option[B], Option[C], Option[D])] =
    map(merge(merge(fa, fb), merge(fc, fd))) { i =>
      (i.left.flatMap(_.left),
       i.left.flatMap(_.right),
       i.right.flatMap(_.left),
       i.right.flatMap(_.right))
    }

}
