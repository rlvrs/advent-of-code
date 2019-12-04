package solution3

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import parser.Parser
import solution3.Solution

abstract class UnitTest extends AnyFunSuite with Matchers

class SolutionShould extends UnitTest {
  test("convert Rn to list of every coordinate from (x,y) to (x+n,y)") {
    val table: List[(Coordinate, String, List[Coordinate])] = List(
      (Coordinate(0,0), "R3", List(Coordinate(1,0),Coordinate(2,0),Coordinate(3,0))),
      (Coordinate(2,0), "R3", List(Coordinate(3,0),Coordinate(4,0),Coordinate(5,0))),
      (Coordinate(0,4), "R3", List(Coordinate(1,4),Coordinate(2,4),Coordinate(3,4))),
      (Coordinate(3,5), "R3", List(Coordinate(4,5),Coordinate(5,5),Coordinate(6,5))),
    )

    table.foreach {
      case (origin, instruction, expected) => Solution.generateCoordinates(origin, instruction) shouldBe expected
      case _ => fail()
    }
  }

  test("convert Un to list of every coordinate from (x,y) to (x,y+n)") {
    val table: List[(Coordinate, String, List[Coordinate])] = List(
      (Coordinate(0,0), "U3", List(Coordinate(0,1),Coordinate(0,2),Coordinate(0,3))),
      (Coordinate(2,0), "U3", List(Coordinate(2,1),Coordinate(2,2),Coordinate(2,3))),
      (Coordinate(0,4), "U3", List(Coordinate(0,5),Coordinate(0,6),Coordinate(0,7))),
      (Coordinate(3,5), "U3", List(Coordinate(3,6),Coordinate(3,7),Coordinate(3,8))),
    )

    table.foreach {
      case (origin, instruction, expected) => Solution.generateCoordinates(origin, instruction) shouldBe expected
      case _ => fail()
    }
  }

  test("convert Dn to list of every coordinate from (x,y) to (x,y-n)") {
    val table: List[(Coordinate, String, List[Coordinate])] = List(
      (Coordinate(0,0), "D3", List(Coordinate(0,-1),Coordinate(0,-2),Coordinate(0,-3))),
      (Coordinate(2,0), "D3", List(Coordinate(2,-1),Coordinate(2,-2),Coordinate(2,-3))),
      (Coordinate(0,4), "D3", List(Coordinate(0,3),Coordinate(0,2),Coordinate(0,1))),
      (Coordinate(3,5), "D3", List(Coordinate(3,4),Coordinate(3,3),Coordinate(3,2))),
    )

    table.foreach {
      case (origin, instruction, expected) => Solution.generateCoordinates(origin, instruction) shouldBe expected
      case _ => fail()
    }
  }

  test("convert Ln to list of every coordinate from (x,y) to (x-n,y)") {
    val table: List[(Coordinate, String, List[Coordinate])] = List(
      (Coordinate(0,0), "L3", List(Coordinate(-1,0),Coordinate(-2,0),Coordinate(-3,0))),
      (Coordinate(2,0), "L3", List(Coordinate(1,0),Coordinate(0,0),Coordinate(-1,0))),
      (Coordinate(0,4), "L3", List(Coordinate(-1,4),Coordinate(-2,4),Coordinate(-3,4))),
      (Coordinate(3,5), "L3", List(Coordinate(2,5),Coordinate(1,5),Coordinate(0,5))),
    )

    table.foreach {
      case (origin, instruction, expected) => Solution.generateCoordinates(origin, instruction) shouldBe expected
      case _ => fail()
    }
  }

  test("convert instructions to list of every coordinates") {
    val table: List[(List[String], List[Coordinate])] = List(
      (List("L3", "U2"),
        List(Coordinate(-1,0),Coordinate(-2,0),Coordinate(-3,0),
          Coordinate(-3,1),Coordinate(-3,2))),
      (List("L3", "U2", "R1"),
        List(Coordinate(-1,0),Coordinate(-2,0),Coordinate(-3,0),
          Coordinate(-3,1),Coordinate(-3,2),
          Coordinate(-2,2)))
    )

    table.foreach {
      case (instructions, expected) => {
        Solution.generateAllCoordinates(instructions) shouldBe expected
      }
      case _ => fail()
    }
  }

  test("return the intersection closer to the origin.") {
    val table: List[(List[Coordinate], List[Coordinate], Int)] = List(
      (Solution.generateAllCoordinates(List("L1", "U2")),
        Solution.generateAllCoordinates(List("U1", "L2")),
        2),
    )

    table.foreach {
      case (wire1, wire2, expected) => Solution.closestManhattanIntersection(wire1, wire2) shouldBe expected
      case _ => fail()
    }
  }

  test("solve part 1: Manhatan distance of the intersection closer to origin") {
    val wiresInstructions: List[List[String]] = Parser.parseLinesAsCommaSeparatedStringList("solution3/input.txt")
    val wiresCoordinates = wiresInstructions
        .map(Solution.generateAllCoordinates)

    val wire1Coordinates = wiresCoordinates.head
    val wire2Coordinates = wiresCoordinates.tail.head

    Solution.closestManhattanIntersection(wire1Coordinates, wire2Coordinates) shouldBe 308
  }

  test("solve part 2: Min signal delay intersection") {
    val wiresInstructions: List[List[String]] = Parser.parseLinesAsCommaSeparatedStringList("solution3/input.txt")
    val wiresCoordinates = wiresInstructions
      .map(Solution.generateAllCoordinates)

    val wire1Coordinates = wiresCoordinates.head
    val wire2Coordinates = wiresCoordinates.tail.head

    Solution.minSignalDelayIntersection(wire1Coordinates, wire2Coordinates) shouldBe 12934
  }
}
