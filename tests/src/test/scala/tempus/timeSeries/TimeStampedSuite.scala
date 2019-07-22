package tempus.timeSeries

import cats.kernel.laws.discipline.OrderTests

class TimeStampedSuite extends TimeSeriesSuite {
  checkAll("TimeStamped", OrderTests[TimeStamped[Int]].order)
}
