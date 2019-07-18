package tempus.timeSeries

import java.time._

import tempus.timeSeries.time.Periodical.{Annually, Daily, Monthly, Quarterly}
import tempus.timeSeries.time.Zone.UTC

import scala.concurrent.duration._

class PeriodicalSeriesSuite extends TimeSeriesSuite {
  test("from TimeSeries") {
    val start = dateTimeOf(1, 2, 12, 20)

    val ts = ListTimeSeries(
      List(TimeStamped(1, start.toInstant),
           TimeStamped(1, start.plusDays(1).toInstant),
           TimeStamped(3, start.plusMonths(1).toInstant)))

    val ps = PeriodicalSeries.from[Monthly, UTC](ts)(_.fold)

    ps shouldBe PeriodicalSeries(
      ListTimeSeries(
        List(
          TimeStamped(2, dateTimeOf(1, 1, 0, 0, zoneId = UTC.id).toInstant),
          TimeStamped(3, dateTimeOf(2, 1, 0, 0, zoneId = UTC.id).toInstant)
        )
      ))
  }

  test("PeriodicalSeries.fill") {
    val start = dateTimeOf(1, 1, year = 2001).toInstant
    val end = dateTimeOf(12, 31, year = 2001).toInstant
    val monthly = PeriodicalSeries.fill[ListTimeSeries, Monthly, UTC](start, end, ())
    monthly.ts shouldBe ListTimeSeries.fromUnOrdered(
      List(
        TimeStamped((), dateTimeOf(1, 1, year = 2001)),
        TimeStamped((), dateTimeOf(2, 1, year = 2001)),
        TimeStamped((), dateTimeOf(3, 1, year = 2001)),
        TimeStamped((), dateTimeOf(4, 1, year = 2001)),
        TimeStamped((), dateTimeOf(5, 1, year = 2001)),
        TimeStamped((), dateTimeOf(6, 1, year = 2001)),
        TimeStamped((), dateTimeOf(7, 1, year = 2001)),
        TimeStamped((), dateTimeOf(8, 1, year = 2001)),
        TimeStamped((), dateTimeOf(9, 1, year = 2001)),
        TimeStamped((), dateTimeOf(10, 1, year = 2001)),
        TimeStamped((), dateTimeOf(11, 1, year = 2001)),
        TimeStamped((), dateTimeOf(12, 1, year = 2001))
      ))

    val quarterly = PeriodicalSeries.fill[ListTimeSeries, Quarterly, UTC](start, end, ())
    quarterly.ts shouldBe ListTimeSeries.fromUnOrdered(
      List(
        TimeStamped((), dateTimeOf(1, 1, year = 2001)),
        TimeStamped((), dateTimeOf(4, 1, year = 2001)),
        TimeStamped((), dateTimeOf(7, 1, year = 2001)),
        TimeStamped((), dateTimeOf(10, 1, year = 2001))
      ))
  }

  test("PeriodicalSeries. instance fill") {
    val spottySeries = PeriodicalSeries.from[Monthly, UTC](
      ListTimeSeries.fromUnOrdered(
        List(
          TimeStamped(1, dateTimeOf(1, 1, year = 2001)),
          TimeStamped(5, dateTimeOf(5, 1, year = 2001)),
          TimeStamped(10, dateTimeOf(8, 1, year = 2001))
        )))(_.fold)

    spottySeries.fill(Some(dateTimeOf(12, 3, year = 2001))).ts shouldBe ListTimeSeries
      .fromUnOrdered(
        List(
          TimeStamped(1, dateTimeOf(1, 1, year = 2001)),
          TimeStamped(1, dateTimeOf(2, 1, year = 2001)),
          TimeStamped(1, dateTimeOf(3, 1, year = 2001)),
          TimeStamped(1, dateTimeOf(4, 1, year = 2001)),
          TimeStamped(5, dateTimeOf(5, 1, year = 2001)),
          TimeStamped(5, dateTimeOf(6, 1, year = 2001)),
          TimeStamped(5, dateTimeOf(7, 1, year = 2001)),
          TimeStamped(10, dateTimeOf(8, 1, year = 2001)),
          TimeStamped(10, dateTimeOf(9, 1, year = 2001)),
          TimeStamped(10, dateTimeOf(10, 1, year = 2001)),
          TimeStamped(10, dateTimeOf(11, 1, year = 2001)),
          TimeStamped(10, dateTimeOf(12, 1, year = 2001))
        ))
  }

  test("PeriodicalSeries#movingWindow Monthly values") {
    val ps = PeriodicalSeries
      .from[Monthly, UTC](
        ListTimeSeries.fromUnOrdered(
          List(
            TimeStamped(1, dateTimeOf(1, 1)),
            TimeStamped(2, dateTimeOf(3, 1)),
            TimeStamped(3, dateTimeOf(7, 1))
          )))(_.fold)
      .fill(Some(dateTimeOf(12, 31)))

    ps.movingWindow[Monthly](4).toList.map(_.toList) shouldBe
      List(
        List(1),
        List(1, 1),
        List(1, 1, 2),
        List(1, 1, 2, 2),
        List(1, 2, 2, 2),
        List(2, 2, 2, 2),
        List(2, 2, 2, 3),
        List(2, 2, 3, 3),
        List(2, 3, 3, 3),
        List(3, 3, 3, 3),
        List(3, 3, 3, 3),
        List(3, 3, 3, 3)
      )
  }
  test("PeriodicalSeries#movingWindow Monthly time and value") {
    val ps = PeriodicalSeries
      .from[Monthly, UTC](
        ListTimeSeries.fromUnOrdered(
          List(
            TimeStamped(1, dateTimeOf(1, 1))
          )))(_.fold)
      .fill(Some(dateTimeOf(4, 30)))

    def fromList[A](
        l: List[(A, Instant)]): PeriodicalSeries[ListTimeSeries, Monthly, UTC, A] =
      PeriodicalSeries.from[Monthly, UTC](
        ListTimeSeries.fromUnOrdered(l.map(TimeStamped(_))))(_.last)

    ps.movingWindow[Monthly](3) shouldBe
      fromList(
        List(
          (fromList(List[(Int, Instant)]((1, dateTimeOf(1)))), dateTimeOf(1): Instant),
          (fromList(List[(Int, Instant)]((1, dateTimeOf(1)), (1, dateTimeOf(2)))),
           dateTimeOf(2): Instant),
          (fromList(
             List[(Int, Instant)]((1, dateTimeOf(1)),
                                  (1, dateTimeOf(2)),
                                  (1, dateTimeOf(3)))),
           dateTimeOf(3): Instant),
          (fromList(
             List[(Int, Instant)]((1, dateTimeOf(2)),
                                  (1, dateTimeOf(3)),
                                  (1, dateTimeOf(4)))),
           dateTimeOf(4): Instant)
        ))
  }

  test("PeriodicalSeries#movingWindow Annually") {
    val ps = PeriodicalSeries
      .from[Monthly, UTC](
        ListTimeSeries.fromUnOrdered(
          List(
            TimeStamped(1, dateTimeOf(1, 1, year = 2001))
          )))(_.fold)
      .fill(Some(dateTimeOf(12, 31, year = 2002)))

    ps.movingWindow[Annually](1).toList.map(_.toList.sum) shouldBe
      (1 to 12).toList ++ List.fill(12)(12)
  }

  test("before") {
    val ps = fromList[Daily, UTC](1 -> 10.days.ago, 2 -> 9.days.ago)
    assertPeriodicalSeriesEq(ps.before(9.days.ago),
                             fromList[Daily, UTC](1 -> 10.days.ago))
  }

  test("from daily data") {
    val ps = PeriodicalSeries.fromDailyData[ListTimeSeries, UTC](
      List('a -> 10.days.ago.date, 'b -> 9.days.ago.date, 'c -> 8.days.ago.date))
    assertPeriodicalSeriesEq(
      ps,
      fromList[Daily, UTC]('a -> 10.days.ago, 'b -> 9.days.ago, 'c -> 8.days.ago))
  }
}
