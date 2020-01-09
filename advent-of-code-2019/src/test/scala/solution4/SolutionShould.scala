package solution4

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

abstract class UnitTest extends AnyFunSuite with Matchers

class SolutionShould extends UnitTest {
  test("filter numbers whose digits from left to right decrease.") {
    val table: List[(SixDigitInt, Boolean)] = List(
      (SixDigitInt(111111), false),
      (SixDigitInt(223450), true),
      (SixDigitInt(123789), false),
    )

    table.foreach {
      case (number, expected) => Solution.isDecreasingDigitsNumber(number) shouldBe expected
      case _ => fail()
    }
  }

  test("filter numbers without same adjacent digits.") {
    val table: List[(SixDigitInt, Boolean)] = List(
      (SixDigitInt(111111), true),
      (SixDigitInt(223450), true),
      (SixDigitInt(123789), false),
    )

    table.foreach {
      case (number, expected) => Solution.hasAtLeastTwoEqualAdjacentDigits(number) shouldBe expected
      case _ => fail()
    }
  }

  test("solve part 1: Counts number of different passwords within range") {
    val range = (372037 to 905157)
      .map(SixDigitInt)
      .toList

    Solution.differentPasswordsWithinRange1(range) shouldBe 481
  }

  test("filter numbers that do not have exactly 2 adjacent digits equal.") {
    val table: List[(SixDigitInt, Boolean)] = List(
      (SixDigitInt(133444), true),
      (SixDigitInt(123444), false),
      (SixDigitInt(377778), false),
      (SixDigitInt(111111), false),
      (SixDigitInt(223450), true),
      (SixDigitInt(123789), false),
      (SixDigitInt(112233), true),
      (SixDigitInt(113444), true),
      (SixDigitInt(111122), true),
    )

    table.foreach {
      case (number, expected) => Solution.hasExactlyTwoEqualAdjacentDigits(number) shouldBe expected
      case _ => fail()
    }
  }

  test("solve part 2: Counts number of different passwords within range") {
    val range = (372037 to 905157)
      .map(SixDigitInt)
      .toList

    Solution.differentPasswordsWithinRange2(range) shouldBe 299
  }
}
