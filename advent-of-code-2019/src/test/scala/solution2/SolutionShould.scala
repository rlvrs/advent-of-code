package solution2

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import parser.Parser

abstract class UnitTest extends AnyFunSuite with Matchers

class SolutionShould extends UnitTest {
  test("perform addition with opcode 1") {
    val initialState = List(1,9,10,3,2,3,11,0,99,30,40,50)
    val expectedState = List(1,9,10,70,2,3,11,0,99,30,40,50)

    Solution.intCodeAdd(0, initialState) shouldBe expectedState
  }

  test("perform multiplication with opcode 2") {
    val initialState = List(1,9,10,70,2,3,11,0,99,30,40,50)
    val expectedState = List(3500,9,10,70,2,3,11,0,99,30,40,50)

    Solution.intCodeMultiply(4, initialState) shouldBe expectedState
  }

  test("execute the whole IntCode program") {
    val initialExpectedStates: List[(List[Int], List[Int])] = List(
      (List(1,1,1,4,99,5,6,0,99), List(30,1,1,4,2,5,6,0,99)),
      (List(1,9,10,3,2,3,11,0,99,30,40,50), List(3500,9,10,70,2,3,11,0,99,30,40,50)),
      (List(2,4,4,5,99,0), List(2,4,4,5,99,9801)),
      (List(2,3,0,3,99), List(2,3,0,6,99)),
      (List(1,0,0,0,99), List(2,0,0,0,99)),
    )

    initialExpectedStates.foreach {
      case (initialState, expectedState) => Solution.executeIntCodeProgram(initialState) shouldBe expectedState
      case _ => fail()
    }
  }

  test("solve part 1: IntCode program should output 4576384 after execution") {
    // Update values come from the assignment.
    val initialState = Parser.parseCommaSeparatedIntList("solution2/input.txt")
      .updated(1, 12)
      .updated(2, 2)

    Solution.executeIntCodeProgram(initialState).head shouldBe 4576384
  }

  test("solve part 2: IntCode program should output 19690720 after execution") {
    val initialState = Parser.parseCommaSeparatedIntList("solution2/input.txt")

    Solution.findComposedNounVerbToOutput(initialState, 19690720) shouldBe 5398
  }
}
