package tempus
package timeSeries

import cats.data.Chain
import cats.kernel.Eq
import org.scalacheck.{Arbitrary, Cogen}
import cats.laws.discipline.arbitrary.catsLawsArbitraryForChain
trait TimeSeriesSuite
    extends TempusSuite {
  implicit def arbListTimeSeries[A: Arbitrary]: Arbitrary[ListTimeSeries[A]] = {
    Arbitrary(
      implicitly[Arbitrary[List[TimeStamped[A]]]].arbitrary
        .map(ListTimeSeries.fromUnOrdered _))
  }

  implicit def arbChainTimeSeries[A: Arbitrary]: Arbitrary[ChainTimeSeries[A]] = {
    Arbitrary(
      implicitly[Arbitrary[Chain[TimeStamped[A]]]].arbitrary
        .map(ChainTimeSeries.fromUnOrdered _))
  }

  implicit def eqListTS[A: Eq]: Eq[ListTimeSeries[A]] = Eq.by(vs => vs.list)
  implicit def eqChainTS[A: Eq]: Eq[ChainTimeSeries[A]] = Eq.by(vs => vs.chain)

  implicit def cogenTimeStamped[A: Cogen]: Cogen[TimeStamped[A]] =
    Cogen[A].contramap(_.v)

  implicit def cogenWithDuration[A: Cogen]: Cogen[WithDuration[A]] =
    Cogen[A].contramap(_.value)

}
