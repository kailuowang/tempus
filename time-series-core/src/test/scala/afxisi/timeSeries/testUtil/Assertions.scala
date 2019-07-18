package afxisi.timeSeries.testUtil

import afxisi.timeSeries.{ListTimeSeries, PeriodicalSeries}
import afxisi.timeSeries.time.{Periodical, Zone}
import cats.data.Ior.Both
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.{Assertion, Matchers}

trait Assertions {
  self: AnyFunSuiteLike with Matchers =>
  def assertPeriodicalSeries[P <: Periodical, Z <: Zone, V](
      valueAssertion: (V, V) => Assertion
  )(ps: PeriodicalSeries[ListTimeSeries, P, Z, V],
    expected: PeriodicalSeries[ListTimeSeries, P, Z, V]) = {
    ps.merge(expected).map {
      case Both(v, exp) => valueAssertion(v, exp)
      case _            => fail(s"two PeriodicalSeries don't match: $ps, $expected")
    }
  }

  def assertPeriodicalSeriesEq[P <: Periodical, Z <: Zone, V] =
    assertPeriodicalSeries[P, Z, V](_ shouldBe _) _

}
