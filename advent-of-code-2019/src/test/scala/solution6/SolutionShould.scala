package solution6

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import parser.Parser

abstract class UnitTest extends AnyFunSuite with Matchers

class SolutionShould extends UnitTest {
  test("solve part 1: calculate number of direct and indirect orbits") {
    val table: List[(String, Int)] = List(
      ("solution6/example_input.txt", 42),
      ("solution6/input.txt", 254447),
    )

    table
      .map(test => (Parser.parseLinesAsAdjacencyList(test._1), test._2))
      .foreach {
      case (adjacencyList, expected) => Solution.calculateNumberOfOrbitsPart1(adjacencyList) shouldBe expected
      case _ => fail()
    }
  }

  test("solve part 2: calculate number of direct and indirect orbits") {
    val table: List[(String, Int)] = List(
      ("solution6/example_input.txt", 3),
      ("solution6/input.txt", 445),
    )

    table
      .map(test => (Parser.parseLinesAsAdjacencyListUndirected(test._1), test._2))
      .foreach {
        case (adjacencyList, expected) => Solution.calculateNumberOfOrbitsPart2(adjacencyList, "YOU", "SAN") shouldBe expected
        case _ => fail()
      }
  }
}
