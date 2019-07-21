package tempus

import java.time._

import tempus.timeSeries.syntax.TimeSeriesSyntax
import tempus.testUtil.{Assertions, Factory}
import tempus.time.{FreePeriod, Periodical}
import cats.kernel.Eq
import cats.tests.CatsSuite
import io.estatico.newtype.Coercible
import org.scalacheck.{Arbitrary, Cogen, Gen}

trait TempusSuite
  extends CatsSuite
    with Factory
    with TimeSeriesSyntax
    with AllSyntax
    with Assertions {
  implicit val aInstant: Arbitrary[Instant] =
    Arbitrary(Gen.choose(0L, 1000000000L).map(Instant.ofEpochSecond))
  implicit val aDuration: Arbitrary[Duration] =
    Arbitrary(Gen.choose(0L, 10000000L).map(Duration.ofSeconds))

  implicit val aArbitraryPeriod: Arbitrary[FreePeriod] =
    Arbitrary(
      for {
        i <- aInstant.arbitrary
        d <- aDuration.arbitrary
      } yield
        FreePeriod(
          start = i,
          duration = d
        ))

  implicit def coercibleArb[A, B](implicit
                                  ev: Coercible[Arbitrary[A], Arbitrary[B]],
                                  A: Arbitrary[A]): Arbitrary[B] = ev(A)

  implicit def eqP[P <: Periodical]: Eq[P] = Eq.fromUniversalEquals[P]

  implicit def cogenPeriod: Cogen[FreePeriod] =
    Cogen[Long].contramap(_.start.getEpochSecond)

  implicit def dateTimeToInstant(dt: ZonedDateTime): Instant = dt.toInstant

}
