package tempus.timeSeries

import tempus.timeSeries.laws.discipline.TimeSeriesTests

class ChainTimeSeriesSuite extends TimeSeriesSuite {
  checkAll("ChainTimeSeries", TimeSeriesTests[ChainTimeSeries].timeSeries[Int, Int, String, Int, Option, Option, List])
}


