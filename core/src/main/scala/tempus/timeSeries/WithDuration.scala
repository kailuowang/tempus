package tempus
package timeSeries

import java.time.Duration

import cats._

case class WithDuration[A](value: A, duration: Duration)

object WithDuration {
  implicit val opFunctor: Functor[WithDuration] = new Functor[WithDuration] {
    def map[A, B](ta: WithDuration[A])(f: A => B): WithDuration[B] =
      WithDuration(f(ta.value), ta.duration)
  }
}
