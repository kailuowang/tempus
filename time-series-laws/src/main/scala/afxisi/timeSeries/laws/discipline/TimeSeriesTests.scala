package afxisi.timeSeries
package laws
package discipline

import java.time.Instant

import cats.data.{Ior, NonEmptyList}
import cats.kernel.CommutativeMonoid
import cats.laws.discipline.TraverseTests
import org.scalacheck.{Arbitrary, Cogen}
import org.scalacheck.Prop._
import cats.{CommutativeApplicative, Eq, Foldable}
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import org.typelevel.discipline.Laws
import cats.instances.list._

trait TimeSeriesTests[F[_]]
    extends MonoidKTests[F]
    with TraverseTests[F]
    with FunctorFilterTests[F] {
  def laws: TimeSeriesLaws[F]

  def timeSeries[A: Arbitrary,
                 B: Arbitrary,
                 C: Arbitrary,
                 M: Arbitrary,
                 X[_]: CommutativeApplicative,
                 Y[_]: CommutativeApplicative,
                 Z[_]: Foldable](implicit
                                 ArbFA: Arbitrary[F[A]],
                                 ArbFOptionA: Arbitrary[F[Option[A]]],
                                 ArbFB: Arbitrary[F[B]],
                                 ArbXB: Arbitrary[X[B]],
                                 ArbXM: Arbitrary[X[M]],
                                 ArbYB: Arbitrary[Y[B]],
                                 ArbYC: Arbitrary[Y[C]],
                                 ArbYM: Arbitrary[Y[M]],
                                 ArbZM: Arbitrary[Z[(A, Instant)]],
                                 ArbFXM: Arbitrary[F[X[M]]],
                                 CogenA: Cogen[A],
                                 CogenB: Cogen[B],
                                 CogenC: Cogen[C],
                                 CogenM: Cogen[M],
                                 M: CommutativeMonoid[M],
                                 MA: CommutativeMonoid[A],
                                 EqFA: Eq[F[A]],
                                 EqFListA: Eq[F[NonEmptyList[A]]],
                                 EqFIorA: Eq[F[A Ior A]],
                                 EqFIorB: Eq[F[A Ior B]],
                                 EqFB: Eq[F[B]],
                                 EqFC: Eq[F[C]],
                                 EqM: Eq[M],
                                 EqA: Eq[A],
                                 EqListA: Eq[List[A]],
                                 EqXYFC: Eq[X[Y[F[C]]]],
                                 EqXFB: Eq[X[F[B]]],
                                 EqYFB: Eq[Y[F[B]]],
                                 EqXFM: Eq[X[F[M]]],
                                 EqYFM: Eq[Y[F[M]]],
                                 EqOptionA: Eq[Option[A]]): RuleSet = new RuleSet {
    val name = "timeSeries"
    val bases: Seq[(String, Laws#RuleSet)] = Seq()
    val parents = Seq(traverse[A, B, C, M, X, Y], monoidK[A], functorFilter[A, B, C])
    val props = Seq(
      "mapWithTime consistent with map" -> forAll(
        laws.mapWithTimeConsistentWithMap[A, B] _),
      "traverseWithTime consistent with traverse" -> forAll(
        laws.traverseWithTimeConsistentWithTraverse[A, B, X] _),
      "groupByTime identity consistent with map" -> forAll(laws.groupByTimeIdentity[A] _),
      "groupByTime preserve sequence within" -> forAll(laws.groupByTimeSequence[A] _),
      "from consistency" -> forAll(laws.fromConsistency[A, Z] _),
      "outer join consistence" -> forAll(laws.outerJoinConsistent[A, B] _),
      "inner join duplication" -> forAll(laws.innerJoinWithDuplication[A, B] _),
      "inner join left only" -> forAll(laws.innerJoinWithLeftOnly[A, B] _),
      "inner join right only" -> forAll(laws.innerJoinWithRightOnly[A, B] _)
    )
  }
}

object TimeSeriesTests {
  def apply[F[_]: TimeSeries]: TimeSeriesTests[F] =
    new TimeSeriesTests[F] {
      def laws: TimeSeriesLaws[F] = TimeSeriesLaws[F]
    }
}
