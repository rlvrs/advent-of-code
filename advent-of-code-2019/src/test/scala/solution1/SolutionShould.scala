package solution1

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import parser.Parser

abstract class UnitTest extends AnyFunSuite with Matchers

class SolutionShould extends UnitTest {
  test("solve part 1: sum fuel requirements") {
    val moduleMasses = Parser.parseLinesAsIntList("solution1/input.txt")
    Solution.calculateSumFuelRequirementsPart1(moduleMasses) shouldBe 3265923
  }

  test("solve part 2: sum real fuel requirements") {
    Solution.calculateSumFuelRequirementsPart2(List(14)) shouldBe 2
    Solution.calculateSumFuelRequirementsPart2(List(1969)) shouldBe 966
    Solution.calculateSumFuelRequirementsPart2(List(100756)) shouldBe 50346

    val moduleMasses = Parser.parseLinesAsIntList("solution1/input.txt")
    Solution.calculateSumFuelRequirementsPart2(moduleMasses) shouldBe 4896020
  }
}
