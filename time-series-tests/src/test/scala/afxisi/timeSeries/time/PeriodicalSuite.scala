package afxisi.timeSeries
package time

import afxisi.timeSeries.time.Periodical._

class PeriodicalSuite extends TimeSeriesSuite {
  test("Monthly periodOf Jan") {
    val period = Period.of[Monthly, Zone.UTC](dateTimeOf(1, 3, 13))

    period.start shouldBe dateTimeOf(1, 1, 0, 0).toInstant
    period.end shouldBe dateTimeOf(2, 1).minusNanos(1).toInstant
  }

  test("Monthly periodOf Feb") {
    val periodFeb = Period.of[Monthly, Zone.UTC](dateTimeOf(2, 3, 13))

    periodFeb.start shouldBe dateTimeOf(2, 1, 0, 0).toInstant
    periodFeb.end shouldBe dateTimeOf(3, 1).minusNanos(1).toInstant

  }

  test("Monthly periodOf Feb in leap year") {
    val periodFebLeap = Period.of[Monthly, Zone.UTC](dateTimeOf(2, 3, 13, year = 2020))

    periodFebLeap.start shouldBe dateTimeOf(2, 1, 0, 0, year = 2020).toInstant
    periodFebLeap.end shouldBe dateTimeOf(2, 29, 23, 59, 59, year = 2020)
      .withNano(999999999)
      .toInstant
  }

  test("Annually periodOf Jan") {
    val period = Period.of[Annually, Zone.UTC](dateTimeOf(1, 3, 13, year = 2010))

    period.start shouldBe dateTimeOf(1, 1, 0, 0, year = 2010).toInstant
    period.end shouldBe dateTimeOf(1, 1, year = 2011).minusNanos(1).toInstant
  }

  test("Daily periodOf Jan") {
    val period = Period.of[Daily, Zone.UTC](dateTimeOf(1, 3, 13))

    period.start shouldBe dateTimeOf(1, 3, 0, 0).toInstant
    period.end shouldBe dateTimeOf(1, 4, 0, 0).minusNanos(1).toInstant
  }

  test("Quarterly periodOf 1/1/1995") {
    Period.of[Quarterly, Zone.UTC](dateTimeOf(1, 1, year = 1995)).end shouldBe dateTimeOf(
      4,
      1,
      year = 1995).toInstant.minusNanos(1)
  }

  test("Annually periodOf 1/1/1995") {
    val period = Period.of[Annually, Zone.UTC](dateTimeOf(1, 1, year = 1995))
    period.end shouldBe dateTimeOf(1, 1, year = 1996).toInstant.minusNanos(1)
    period.duration.getSeconds shouldBe 365 * 24 * 3600 - 1
  }

  test("Quarterly periodOf") {

    def checkQuarter(month: Int, expectedStartMonth: Int, expectedEndMonth: Int) = {
      val period = Period.of[Quarterly, Zone.UTC](dateTimeOf(month, 3, 13))

      period.start shouldBe dateTimeOf(expectedStartMonth, 1, 0, 0).toInstant
      period.end shouldBe dateTimeOf(expectedEndMonth, 1, 0, 0)
        .plusMonths(1)
        .minusNanos(1)
        .toInstant
    }

    checkQuarter(1, 1, 3)
    checkQuarter(2, 1, 3)
    checkQuarter(3, 1, 3)
    checkQuarter(4, 4, 6)
    checkQuarter(5, 4, 6)
    checkQuarter(6, 4, 6)
    checkQuarter(7, 7, 9)
    checkQuarter(8, 7, 9)
    checkQuarter(9, 7, 9)
    checkQuarter(10, 10, 12)
    checkQuarter(11, 10, 12)
    checkQuarter(12, 10, 12)
  }

  test("Monthly next") {
    Period.of[Monthly, Zone.UTC](dateTimeOf(2, 1)).next shouldBe Period
      .of[Monthly, Zone.UTC](dateTimeOf(3, 1))
  }

  test("Monthly previous") {
    Period.of[Monthly, Zone.UTC](dateTimeOf(2, 1)).previous shouldBe Period
      .of[Monthly, Zone.UTC](dateTimeOf(1, 1))
  }

  test("Monthly nPeriodicalAway past") {
    Period
      .of[Monthly, Zone.UTC](dateTimeOf(2, 1, year = 2000))
      .nPeriodicalAway[Annually](-5) shouldBe
      Period.of[Monthly, Zone.UTC](dateTimeOf(2, 1, year = 1995))
  }

  test("Monthly nPeriodicalAway future") {
    Period
      .of[Monthly, Zone.UTC](dateTimeOf(2, 1, year = 2000))
      .nPeriodicalAway[Annually](3) shouldBe
      Period.of[Monthly, Zone.UTC](dateTimeOf(2, 1, year = 2003))
  }

  test("Monthly nPeriodicalAway same") {
    Period
      .of[Monthly, Zone.UTC](dateTimeOf(2, 1, year = 2000))
      .nPeriodicalAway[Annually](0) shouldBe
      Period.of[Monthly, Zone.UTC](dateTimeOf(2, 1, year = 2000))
  }

  test("Monthly nPeriodicalAway same month") {
    Period
      .of[Monthly, Zone.UTC](dateTimeOf(2, 1, year = 2000))
      .nPeriodicalAway[Monthly](0) shouldBe
      Period.of[Monthly, Zone.UTC](dateTimeOf(2, 1, year = 2000))
  }
  test("Monthly nPeriodicalAway 1 month") {
    Period
      .of[Monthly, Zone.UTC](dateTimeOf(2, 1, year = 2000))
      .nPeriodicalAway[Monthly](1) shouldBe
      Period.of[Monthly, Zone.UTC](dateTimeOf(3, 1, year = 2000))
  }

  test("Quarterly nPeriodicalAway past") {
    val r = Period
      .of[Quarterly, Zone.UTC](dateTimeOf(2, 1, year = 2000))
      .nPeriodicalAway[Annually](-5)
    r.start shouldBe dateTimeOf(1, 1, year = 1995).toInstant
    r.end shouldBe dateTimeOf(4, 1, year = 1995).toInstant.minusNanos(1)
  }

  test("Quarterly nPeriodicalAway future") {
    Period
      .of[Quarterly, Zone.UTC](dateTimeOf(1, 1, year = 2000))
      .nPeriodicalAway[Annually](3) shouldBe
      Period.of[Quarterly, Zone.UTC](dateTimeOf(1, 1, year = 2003))
  }

  test("Period ofPortion") {
    Period
      .of[Monthly, Zone.UTC](dateTimeOf(4, 1, year = 2001))
      .portionOf[Annually] shouldBe 0.0821918 +- 0.00001
  }
}
