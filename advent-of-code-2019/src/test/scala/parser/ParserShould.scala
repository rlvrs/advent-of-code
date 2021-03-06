package parser

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

abstract class UnitTest extends AnyFunSuite with Matchers

class ParserShould extends UnitTest {
  test("return positive number if preceded by +") {
    Parser.parseSignedNumber("+1") shouldBe Some(1)
    Parser.parseSignedNumber(" +24") shouldBe Some(24)
    Parser.parseSignedNumber("+31 ") shouldBe Some(31)
  }

  test("return positive number if preceded by -") {
    Parser.parseSignedNumber("-1") shouldBe Some(-1)
    Parser.parseSignedNumber("-24 ") shouldBe Some(-24)
    Parser.parseSignedNumber(" -31") shouldBe Some(-31)
  }

  test("return the 0 number") {
    Parser.parseSignedNumber("0") shouldBe Some(0)
  }

  test("return None if it is not a number") {
    Parser.parseSignedNumber("     ") shouldBe None
    Parser.parseSignedNumber("asdasd") shouldBe None
    Parser.parseSignedNumber("qwerty") shouldBe None
  }

  test("return list of integers from comma separated text file") {
    val expectedList = List(1,9,10,3,2,3,11,0,99,30,40,50)
    Parser.parseCommaSeparatedIntList("parser/comma_separated_file.txt") shouldBe expectedList
  }
}
