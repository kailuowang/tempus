package tempus.timeSeries.time

import cats.kernel.laws.discipline.PartialOrderTests
import tempus.timeSeries.TimeSeriesSuite

class FreePeriodSuite extends TimeSeriesSuite {
  checkAll("Period", PartialOrderTests[FreePeriod].partialOrder)
}
