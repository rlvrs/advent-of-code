package solution7

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import parser.Parser
import solution7.IntCodeState.{InstructionPointer, Memory, OutputRegs}

abstract class UnitTest extends AnyFunSuite with Matchers

class IntCodeShould extends UnitTest {
  test("perform addition using different parameter modes") {
    val table: List[(Memory, InstructionPointer, (Memory, InstructionPointer))] = List(
      (List(1,9,10,3,2,3,11,0,99,30,40,50), 0, (List(1,9,10,70,2,3,11,0,99,30,40,50), 4)),

      (List(101,9,10,3,2,3,11,0,99,30,40,50), 0, (List(101,9,10,49,2,3,11,0,99,30,40,50), 4)),

      (List(1001,9,10,3,2,3,11,0,99,30,40,50), 0, (List(1001,9,10,40,2,3,11,0,99,30,40,50), 4)),

      (List(1101,9,10,3), 0, (List(1101,9,10,19), 4)),
      (List(1101,-9,10,3), 0, (List(1101,-9,10,1), 4)),
    )

    table.foreach {
      case (initialMemory, instructionPointer, expected) =>
        Instruction(initialMemory(instructionPointer)) match {
          case Some(Add(p1, p2, _)) => IntCode.intCodeAdd(initialMemory, instructionPointer, p1, p2) shouldBe expected
          case _ => fail()
        }
      case _ => fail()
    }
  }

  test("perform multiplication using different parameter modes") {
    val table: List[(Memory, InstructionPointer, (Memory, InstructionPointer))] = List(
      (List(1101,9,10,70,2,3,11,0,99,30,40,50), 4, (List(3500,9,10,70,2,3,11,0,99,30,40,50), 8)),
      (List(1101,9,10,-70,2,3,11,0,99,30,40,50), 4, (List(-3500,9,10,-70,2,3,11,0,99,30,40,50), 8)),

      (List(1101,9,10,70,1002,3,11,0,99,30,40,50), 4, (List(770,9,10,70,1002,3,11,0,99,30,40,50), 8)),

      (List(1101,9,10,70,102,3,11,0,99,30,40,50), 4, (List(150,9,10,70,102,3,11,0,99,30,40,50), 8)),

      (List(1101,9,10,70,1102,3,11,0,99,30,40,50), 4, (List(33,9,10,70,1102,3,11,0,99,30,40,50), 8)),
    )

    table.foreach {
      case (initialMemory, instructionPointer, expected) =>
        Instruction(initialMemory(instructionPointer)) match {
          case Some(Multiply(p1, p2, _)) => IntCode.intCodeMultiply(initialMemory, instructionPointer, p1, p2) shouldBe expected
          case _ => fail()
        }
      case _ => fail()
    }
  }

  test("perform input using position mode only") {
    val initialMemory = List(3,0,4,0,99)
    val valToWrite = 1
    val expectedState = (List(1,0,4,0,99), 2)

    IntCode.intCodeInput(initialMemory, 0, valToWrite) shouldBe expectedState
  }

  test("perform output using different parameter modes") {
    val table: List[(Memory, InstructionPointer, (Memory, InstructionPointer, OutputRegs))] = List(
      (List(3,0,4,0,99), 2, (List(3,0,4,0,99), 4, List(3))),

      (List(3,0,104,0,99), 2, (List(3,0,104,0,99), 4, List(0))),
    )

    table.foreach {
      case (initialMemory, instructionPointer, expected) =>
        Instruction(initialMemory(instructionPointer)) match {
          case Some(Output(p1)) => IntCode.intCodeOutput(initialMemory, instructionPointer, p1, List()) shouldBe expected
          case _ => fail()
        }
      case _ => fail()
    }
  }

  test("execute the whole IntCode program") {
    val table: List[(Memory, Memory, OutputRegs)] = List(
      (List(1,1,1,4,99,5,6,0,99), List(30,1,1,4,2,5,6,0,99), List()),
      (List(1,9,10,3,2,3,11,0,99,30,40,50), List(3500,9,10,70,2,3,11,0,99,30,40,50), List()),
      (List(2,4,4,5,99,0), List(2,4,4,5,99,9801), List()),
      (List(2,3,0,3,99), List(2,3,0,6,99), List()),
      (List(1,0,0,0,99), List(2,0,0,0,99), List()),
    )

    table.foreach {
      case (initialMemory, expectedState, expectedOutputs) =>
        val newIntCodeState = IntCode.executeIntCodeProgram(
          IntCodeState(initialMemory, 0, Registers(List(0), List()), Paused)
        )
        newIntCodeState.memory shouldBe expectedState
        newIntCodeState.registers.output shouldBe expectedOutputs
      case _ => fail()
    }
  }

  test("IntCode program should output 11193703 after execution") {
    // Update values come from the assignment.
    val initialMemory = Parser.parseCommaSeparatedIntList("solution5/input.txt")

    // Last print is 11193703.
    val newIntCodeState = IntCode.executeIntCodeProgram(
      IntCodeState(initialMemory, 0, Registers(List(1), List()), Paused)
    )
    newIntCodeState.memory.head shouldBe 3
    newIntCodeState.registers.output.last shouldBe 11193703
  }

  test("perform jump if true using different parameter modes") {
    val table: List[(Memory, InstructionPointer, (Memory, InstructionPointer))] = List(
      (List(105,9,5,99,-1,4), 0, (List(105,9,5,99,-1,4), 4)),
      (List(105,-1,5,99,-1,4), 0, (List(105,-1,5,99,-1,4), 4)),
      (List(105,0,5,99,-1,4), 0, (List(105,0,5,99,-1,4), 3)),

      (List(5,4,5,99,9,4), 0, (List(5,4,5,99,9,4), 4)),
      (List(5,4,5,99,-1,4), 0, (List(5,4,5,99,-1,4), 4)),
      (List(5,4,5,99,0,4), 0, (List(5,4,5,99,0,4), 3)),

      (List(1005,4,5,99,9,4), 0, (List(1005,4,5,99,9,4), 5)),
      (List(1005,4,5,99,-1,4), 0, (List(1005,4,5,99,-1,4), 5)),
      (List(1005,4,5,99,0,4), 0, (List(1005,4,5,99,0,4), 3)),

      (List(1105,9,5,99,-1,4), 0, (List(1105,9,5,99,-1,4), 5)),
      (List(1105,-1,5,99,-1,4), 0, (List(1105,-1,5,99,-1,4), 5)),
      (List(1105,0,5,99,-1,4), 0, (List(1105,0,5,99,-1,4), 3)),
    )

    table.foreach {
      case (initialMemory, instructionPointer, expected) =>
        Instruction(initialMemory(instructionPointer)) match {
          case Some(JumpIfTrue(p1, p2)) => IntCode.intCodeJumpIfTrue(initialMemory, instructionPointer, p1, p2) shouldBe expected
          case _ => fail()
        }
      case _ => fail()
    }
  }

  test("perform jump if false using different parameter modes") {
    val table: List[(Memory, InstructionPointer, (Memory, InstructionPointer))] = List(
      (List(106,9,5,99,-1,4), 0, (List(106,9,5,99,-1,4), 3)),
      (List(106,-1,5,99,-1,4), 0, (List(106,-1,5,99,-1,4), 3)),
      (List(106,0,5,99,-1,4), 0, (List(106,0,5,99,-1,4), 4)),

      (List(6,4,5,99,9,4), 0, (List(6,4,5,99,9,4), 3)),
      (List(6,4,5,99,-1,4), 0, (List(6,4,5,99,-1,4), 3)),
      (List(6,4,5,99,0,4), 0, (List(6,4,5,99,0,4), 4)),

      (List(1006,4,5,99,9,4), 0, (List(1006,4,5,99,9,4), 3)),
      (List(1006,4,5,99,-1,4), 0, (List(1006,4,5,99,-1,4), 3)),
      (List(1006,4,5,99,0,4), 0, (List(1006,4,5,99,0,4), 5)),

      (List(1106,9,5,99,-1,4), 0, (List(1106,9,5,99,-1,4), 3)),
      (List(1106,-1,5,99,-1,4), 0, (List(1106,-1,5,99,-1,4), 3)),
      (List(1106,0,5,99,-1,4), 0, (List(1106,0,5,99,-1,4), 5)),
    )

    table.foreach {
      case (initialMemory, instructionPointer, expected) =>
        Instruction(initialMemory(instructionPointer)) match {
          case Some(JumpIfFalse(p1, p2)) => IntCode.intCodeJumpIfFalse(initialMemory, instructionPointer, p1, p2) shouldBe expected
          case _ => fail()
        }
      case _ => fail()
    }
  }

  test("perform less than using different parameter modes") {
    val table: List[(Memory, InstructionPointer, (Memory, InstructionPointer))] = List(
      (List(107,9,5,6,99,-1,4), 0, (List(107,9,5,6,99,-1,0), 4)),
      (List(107,-1,5,6,99,-1,4), 0, (List(107,-1,5,6,99,-1,0), 4)),
      (List(107,-3,5,6,99,-1,4), 0, (List(107,-3,5,6,99,-1,1), 4)),

      (List(7,4,5,6,11,9,4), 0, (List(7,4,5,6,11,9,0), 4)),
      (List(7,4,5,6,-10,-1,4), 0, (List(7,4,5,6,-10,-1,1), 4)),
      (List(7,4,5,6,-1,0,4), 0, (List(7,4,5,6,-1,0,1), 4)),

      (List(1007,4,5,6,11,9,4), 0, (List(1007,4,5,6,11,9,0), 4)),
      (List(1007,4,5,6,-10,-1,4), 0, (List(1007,4,5,6,-10,-1,1), 4)),
      (List(1007,4,5,6,-1,0,4), 0, (List(1007,4,5,6,-1,0,1), 4)),

      (List(1107,9,5,6,99,-1,4), 0, (List(1107,9,5,6,99,-1,0), 4)),
      (List(1107,-1,5,6,99,-1,4), 0, (List(1107,-1,5,6,99,-1,1), 4)),
      (List(1107,0,5,6,99,-1,4), 0, (List(1107,0,5,6,99,-1,1), 4)),
    )

    table.foreach {
      case (initialMemory, instructionPointer, expected) =>
        Instruction(initialMemory(instructionPointer)) match {
          case Some(LessThan(p1, p2, _)) => IntCode.intCodeLessThan(initialMemory, instructionPointer, p1, p2) shouldBe expected
          case _ => fail()
        }
      case _ => fail()
    }
  }

  test("perform equals using different parameter modes") {
    val table: List[(Memory, InstructionPointer, (Memory, InstructionPointer))] = List(
      (List(108,9,5,6,99,-1,4), 0, (List(108,9,5,6,99,-1,0), 4)),
      (List(108,-1,5,6,99,-1,4), 0, (List(108,-1,5,6,99,-1,1), 4)),
      (List(108,-3,5,6,99,-1,4), 0, (List(108,-3,5,6,99,-1,0), 4)),

      (List(8,4,5,6,11,9,4), 0, (List(8,4,5,6,11,9,0), 4)),
      (List(8,4,5,6,-10,-1,4), 0, (List(8,4,5,6,-10,-1,0), 4)),
      (List(8,4,5,6,0,0,4), 0, (List(8,4,5,6,0,0,1), 4)),

      (List(1008,4,5,6,11,9,4), 0, (List(1008,4,5,6,11,9,0), 4)),
      (List(1008,4,5,6,5,-1,4), 0, (List(1008,4,5,6,5,-1,1), 4)),
      (List(1008,4,5,6,0,0,4), 0, (List(1008,4,5,6,0,0,0), 4)),

      (List(1108,9,9,6,99,-1,4), 0, (List(1108,9,9,6,99,-1,1), 4)),
      (List(1108,-1,5,6,99,-1,4), 0, (List(1108,-1,5,6,99,-1,0), 4)),
      (List(1108,0,5,6,99,-1,4), 0, (List(1108,0,5,6,99,-1,0), 4)),
    )

    table.foreach {
      case (initialMemory, instructionPointer, expected) =>
        Instruction(initialMemory(instructionPointer)) match {
          case Some(Equals(p1, p2, _)) => IntCode.intCodeEquals(initialMemory, instructionPointer, p1, p2) shouldBe expected
          case _ => fail()
        }
      case _ => fail()
    }
  }

  test("IntCode program should output 12410607 after execution") {
    // Update values come from the assignment.
    val initialMemory = Parser.parseCommaSeparatedIntList("solution5/input.txt")

    // Last print is 12410607.
    val newIntCodeState = IntCode.executeIntCodeProgram(
      IntCodeState(initialMemory, 0, Registers(List(5), List()), Paused)
    )
    newIntCodeState.memory.head shouldBe 314
    newIntCodeState.registers.output.last shouldBe 12410607
  }

  test("solve part 1: IntCode program to calculate max signal to thrusters.") {
    // Update values come from the assignment.
    val initialMemory = Parser.parseCommaSeparatedIntList("solution7/input.txt")

    Solution.maximizeThrustersInputPart1(initialMemory, 0 to 4) shouldBe 272368
  }

  test("solve part 2: IntCode program to calculate max signal to thrusters feedback loop.") {
    val table: List[(String, Int)] = List(
      ("solution7/input_part2_1.txt", 139629729),
      ("solution7/input_part2_2.txt", 18216),
      ("solution7/input.txt", 19741286),
    )

    table.foreach {
      case (resourcePath, expectedOutput) =>
        val initialMemory = Parser.parseCommaSeparatedIntList(resourcePath)
        Solution.maximizeThrustersInputPart2(initialMemory, 5 to 9) shouldBe expectedOutput

      case _ => fail()
    }
  }
}
