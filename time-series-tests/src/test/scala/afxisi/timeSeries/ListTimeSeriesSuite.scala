package afxisi
package timeSeries

import afxisi.timeSeries.laws.discipline.TimeSeriesTests
import afxisi.timeSeries.time._
import cats.kernel.laws.discipline._

class ListTimeSeriesSuite extends TimeSeriesSuite {
  checkAll("ListTimeSeries", TimeSeriesTests[ListTimeSeries].timeSeries[Int, Int, String, Int, Option, Option, List])

  checkAll("Period", PartialOrderTests[FreePeriod].partialOrder)

  checkAll("TimeStamped", OrderTests[TimeStamped[Int]].order)
}


