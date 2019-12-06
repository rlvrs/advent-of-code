package solution5

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import parser.Parser

abstract class UnitTest extends AnyFunSuite with Matchers

class SolutionShould extends UnitTest {
  test("perform addition of arg1(immediate), arg2(immediate) and store in position mode") {
    val initialState = List(1101,9,10,3,2,3,11,0,99,30,40,50)
    val expectedState = (List(1101,9,10,19,2,3,11,0,99,30,40,50), 4)

    Solution.intCodeAdd(initialState, 0) shouldBe expectedState
  }

  test("perform addition of arg1(position), arg2(position) and store in position mode") {
    val initialState = List(1,9,10,3,2,3,11,0,99,30,40,50)
    val expectedState = (List(1,9,10,70,2,3,11,0,99,30,40,50), 4)

    Solution.intCodeAdd(initialState, 0) shouldBe expectedState
  }

  test("perform addition of arg1(immediate), arg2(position) and store in position mode") {
    val initialState = List(101,9,10,3,2,3,11,0,99,30,40,50)
    val expectedState = (List(101,9,10,49,2,3,11,0,99,30,40,50), 4)

    Solution.intCodeAdd(initialState, 0) shouldBe expectedState
  }

  test("perform addition of arg1(position), arg2(immediate) and store in position mode") {
    val initialState = List(1001,9,10,3,2,3,11,0,99,30,40,50)
    val expectedState = (List(1001,9,10,40,2,3,11,0,99,30,40,50), 4)

    Solution.intCodeAdd(initialState, 0) shouldBe expectedState
  }

  test("perform multiplication of arg1(position), arg2(position) and store in position mode") {
    val initialState = List(1101,9,10,70,2,3,11,0,99,30,40,50)
    val expectedState = (List(3500,9,10,70,2,3,11,0,99,30,40,50), 8)

    Solution.intCodeMultiply(initialState, 4) shouldBe expectedState
  }

  test("perform multiplication of arg1(position), arg2(immediate) and store in position mode") {
    val initialState = List(1101,9,10,70,1002,3,11,0,99,30,40,50)
    val expectedState = (List(770,9,10,70,1002,3,11,0,99,30,40,50), 8)

    Solution.intCodeMultiply(initialState, 4) shouldBe expectedState
  }

  test("perform multiplication of arg1(immediate), arg2(position) and store in position mode") {
    val initialState = List(1101,9,10,70,102,3,11,0,99,30,40,50)
    val expectedState = (List(150,9,10,70,102,3,11,0,99,30,40,50), 8)

    Solution.intCodeMultiply(initialState, 4) shouldBe expectedState
  }

  test("perform multiplication of arg1(immediate), arg2(immediate) and store in position mode") {
    val initialState = List(1101,9,10,70,1102,3,11,0,99,30,40,50)
    val expectedState = (List(33,9,10,70,1102,3,11,0,99,30,40,50), 8)

    Solution.intCodeMultiply(initialState, 4) shouldBe expectedState
  }

  test("perform input with opcode 3") {
    val initialState = List(3,0,4,0,99)
    val valToWrite = 1
    val expectedState = (List(1,0,4,0,99), 2)

    Solution.intCodeInput(initialState, 0, valToWrite) shouldBe expectedState
  }

  test("perform output with opcode 4") {
    val initialState = List(3,0,4,0,99)
    val expectedState = (List(3,0,4,0,99), 4)

    Solution.intCodeOutput(initialState, 2) shouldBe expectedState
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

  test("solve part 1: IntCode program should output 11193703 after execution") {
    // Update values come from the assignment.
    val initialState = Parser.parseCommaSeparatedIntList("solution5/input.txt")

    // Last print is 11193703.
    Solution.executeIntCodeProgram(initialState, 1).head shouldBe 3
  }

  test("perform jump if true with different parameter modes") {
    val table: List[(List[Int], (List[Int], Int))] = List(
      (List(105,9,5,99,-1,4), (List(105,9,5,99,-1,4), 4)),
      (List(105,-1,5,99,-1,4), (List(105,-1,5,99,-1,4), 4)),
      (List(105,0,5,99,-1,4), (List(105,0,5,99,-1,4), 3)),

      (List(5,4,5,99,9,4), (List(5,4,5,99,9,4), 4)),
      (List(5,4,5,99,-1,4), (List(5,4,5,99,-1,4), 4)),
      (List(5,4,5,99,0,4), (List(5,4,5,99,0,4), 3)),

      (List(1005,4,5,99,9,4), (List(1005,4,5,99,9,4), 5)),
      (List(1005,4,5,99,-1,4), (List(1005,4,5,99,-1,4), 5)),
      (List(1005,4,5,99,0,4), (List(1005,4,5,99,0,4), 3)),

      (List(1105,9,5,99,-1,4), (List(1105,9,5,99,-1,4), 5)),
      (List(1105,-1,5,99,-1,4), (List(1105,-1,5,99,-1,4), 5)),
      (List(1105,0,5,99,-1,4), (List(1105,0,5,99,-1,4), 3)),
    )

    table.foreach {
      case (programState, expected) => Solution.intCodeJumpIfTrue(programState, 0) shouldBe expected
      case _ => fail()
    }
  }

  test("perform jump if false with different parameter modes") {
    val table: List[(List[Int], (List[Int], Int))] = List(
      (List(106,9,5,99,-1,4), (List(106,9,5,99,-1,4), 3)),
      (List(106,-1,5,99,-1,4), (List(106,-1,5,99,-1,4), 3)),
      (List(106,0,5,99,-1,4), (List(106,0,5,99,-1,4), 4)),

      (List(6,4,5,99,9,4), (List(6,4,5,99,9,4), 3)),
      (List(6,4,5,99,-1,4), (List(6,4,5,99,-1,4), 3)),
      (List(6,4,5,99,0,4), (List(6,4,5,99,0,4), 4)),

      (List(1006,4,5,99,9,4), (List(1006,4,5,99,9,4), 3)),
      (List(1006,4,5,99,-1,4), (List(1006,4,5,99,-1,4), 3)),
      (List(1006,4,5,99,0,4), (List(1006,4,5,99,0,4), 5)),

      (List(1106,9,5,99,-1,4), (List(1106,9,5,99,-1,4), 3)),
      (List(1106,-1,5,99,-1,4), (List(1106,-1,5,99,-1,4), 3)),
      (List(1106,0,5,99,-1,4), (List(1106,0,5,99,-1,4), 5)),
    )

    table.foreach {
      case (programState, expected) => Solution.intCodeJumpIfFalse(programState, 0) shouldBe expected
      case _ => fail()
    }
  }

  test("perform less than with different parameter modes") {
    val table: List[(List[Int], (List[Int], Int))] = List(
      (List(107,9,5,6,99,-1,4), (List(107,9,5,6,99,-1,0), 4)),
      (List(107,-1,5,6,99,-1,4), (List(107,-1,5,6,99,-1,0), 4)),
      (List(107,-3,5,6,99,-1,4), (List(107,-3,5,6,99,-1,1), 4)),

      (List(7,4,5,6,11,9,4), (List(7,4,5,6,11,9,0), 4)),
      (List(7,4,5,6,-10,-1,4), (List(7,4,5,6,-10,-1,1), 4)),
      (List(7,4,5,6,-1,0,4), (List(7,4,5,6,-1,0,1), 4)),

      (List(1007,4,5,6,11,9,4), (List(1007,4,5,6,11,9,0), 4)),
      (List(1007,4,5,6,-10,-1,4), (List(1007,4,5,6,-10,-1,1), 4)),
      (List(1007,4,5,6,-1,0,4), (List(1007,4,5,6,-1,0,1), 4)),

      (List(1107,9,5,6,99,-1,4), (List(1107,9,5,6,99,-1,0), 4)),
      (List(1107,-1,5,6,99,-1,4), (List(1107,-1,5,6,99,-1,1), 4)),
      (List(1107,0,5,6,99,-1,4), (List(1107,0,5,6,99,-1,1), 4)),
    )

    table.foreach {
      case (programState, expected) => Solution.intCodeLessThan(programState, 0) shouldBe expected
      case _ => fail()
    }
  }

  test("perform equals with different parameter modes") {
    val table: List[(List[Int], (List[Int], Int))] = List(
      (List(108,9,5,6,99,-1,4), (List(108,9,5,6,99,-1,0), 4)),
      (List(108,-1,5,6,99,-1,4), (List(108,-1,5,6,99,-1,1), 4)),
      (List(108,-3,5,6,99,-1,4), (List(108,-3,5,6,99,-1,0), 4)),

      (List(8,4,5,6,11,9,4), (List(8,4,5,6,11,9,0), 4)),
      (List(8,4,5,6,-10,-1,4), (List(8,4,5,6,-10,-1,0), 4)),
      (List(8,4,5,6,0,0,4), (List(8,4,5,6,0,0,1), 4)),

      (List(1008,4,5,6,11,9,4), (List(1008,4,5,6,11,9,0), 4)),
      (List(1008,4,5,6,5,-1,4), (List(1008,4,5,6,5,-1,1), 4)),
      (List(1008,4,5,6,0,0,4), (List(1008,4,5,6,0,0,0), 4)),

      (List(1108,9,9,6,99,-1,4), (List(1108,9,9,6,99,-1,1), 4)),
      (List(1108,-1,5,6,99,-1,4), (List(1108,-1,5,6,99,-1,0), 4)),
      (List(1108,0,5,6,99,-1,4), (List(1108,0,5,6,99,-1,0), 4)),
    )

    table.foreach {
      case (programState, expected) => Solution.intCodeEquals(programState, 0) shouldBe expected
      case _ => fail()
    }
  }

  test("solve part 2: IntCode program should output 12410607 after execution") {
    // Update values come from the assignment.
    val initialState = Parser.parseCommaSeparatedIntList("solution5/input.txt")

    // Last print is 12410607.
    Solution.executeIntCodeProgram(initialState, 5).head shouldBe 314
  }
}
