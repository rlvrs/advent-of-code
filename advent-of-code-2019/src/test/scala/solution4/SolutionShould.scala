package solution4

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

abstract class UnitTest extends AnyFunSuite with Matchers

class SolutionShould extends UnitTest {
  test("filter numbers whose digits from left to right decrease.") {
    val table: List[(Int, Boolean)] = List(
      (111111, false),
      (223450, true),
      (123789, false),
    )

    table.foreach {
      case (number, expected) => Solution.isDecreasingDigitsNumber(number) shouldBe expected
      case _ => fail()
    }
  }

  test("filter numbers without same adjacent digits.") {
    val table: List[(Int, Boolean)] = List(
      (111111, true),
      (223450, true),
      (123789, false),
    )

    table.foreach {
      case (number, expected) => Solution.hasAtLeastTwoEqualAdjacentDigits(number) shouldBe expected
      case _ => fail()
    }
  }

  test("solve part 1: Counts number of different passwords within range") {
    val range = Range(372037, 905157).toList

    Solution.differentPasswordsWithinRange1(range) shouldBe 481
  }

  test("filter numbers that do not have exactly 2 adjacent digits equal.") {
    val table: List[(Int, Boolean)] = List(
      (133444, true),
      (123444, false),
      (377778, false),
      (111111, false),
      (223450, true),
      (123789, false),
      (112233, true),
      (113444, true),
      (111122, true),
    )

    table.foreach {
      case (number, expected) => Solution.hasExactlyTwoEqualAdjacentDigits(number) shouldBe expected
      case _ => fail()
    }
  }

  test("solve part 2: Counts number of different passwords within range") {
    val range = Range(372037, 905157).toList

    Solution.differentPasswordsWithinRange2(range) shouldBe 299
  }
}
