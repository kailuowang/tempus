package tempus.timeSeries

package bench

import cats.implicits._
import implicits._
import java.time.Instant

import cats.Functor
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

@State(Scope.Benchmark)
class TimeStampedBench {

  val ts = TimeStamped(1.0, Instant.now)

  @Benchmark
  def mapSyntax(): TimeStamped[String] = ts.map(_.show)

  @Benchmark
  def reconstruct(): TimeStamped[String] = TimeStamped(ts.v.show, ts.time)
}
