package tempus
package timeSeries

import tempus.timeSeries.laws.discipline.TimeSeriesTests

class ListTimeSeriesSuite extends TimeSeriesSuite {
  checkAll("ListTimeSeries", TimeSeriesTests[ListTimeSeries].timeSeries[Int, Int, String, Int, Option, Option, List])
}


