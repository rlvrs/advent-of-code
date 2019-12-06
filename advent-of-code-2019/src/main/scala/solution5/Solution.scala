package solution5

// TODO IntCodeInstruction, IntCodeInstructionAdd, IntCodeInstructionMultiply, ...
// TODO Can I generalise the pattern matches that seem to have duplicated code?

object Solution {
  def executeIntCodeProgram(programState: List[Int], valToWrite: Int=0): List[Int] = {
    @scala.annotation.tailrec
    def loop(programState: List[Int], instructionPointer: Int): List[Int] = {
      val instructionCode = programState(instructionPointer)
      val (newProgramState, newInstructionPointer) = instructionCode.toString match {
        case "99" => (programState, -1)
        case s"${_}01" | "1" => intCodeAdd(programState, instructionPointer)
        case s"${_}02" | "2" => intCodeMultiply(programState, instructionPointer)
        case "3" => intCodeInput(programState, instructionPointer, valToWrite)
        case s"${_}04" | "4" => intCodeOutput(programState, instructionPointer)
        case s"${_}05" | "5" => intCodeJumpIfTrue(programState, instructionPointer)
        case s"${_}06" | "6" => intCodeJumpIfFalse(programState, instructionPointer)
        case s"${_}07" | "7" => intCodeLessThan(programState, instructionPointer)
        case s"${_}08" | "8" => intCodeEquals(programState, instructionPointer)
        case _ => (List(), -1)
      }
      if (newInstructionPointer < 0) {
        newProgramState
      } else {
        loop(newProgramState, newInstructionPointer)
      }
    }

    loop(programState, 0)
  }

  def intCodeArithmeticOperation(getArg1: (List[Int], Int) => Int)(getArg2: (List[Int], Int) => Int)(operation: (Int, Int) => Int)(programState: List[Int], instructionPointer: Int): (List[Int], Int) = {
    val firstOperand = getArg1(programState, instructionPointer+1)
    val secondOperand = getArg2(programState, instructionPointer+2)
    val resultIndex = programState(instructionPointer+3)

    val result = operation(firstOperand, secondOperand)

    (programState.updated(resultIndex, result), instructionPointer+4)
  }

  def intCodeMultiply(programState: List[Int], instructionPointer: Int): (List[Int], Int) = {
    def multiplication(a: Int, b: Int): Int = a * b
    programState(instructionPointer) match {
      case 2 => intCodeArithmeticOperation(getPositionParameter)(getPositionParameter)(multiplication)(programState, instructionPointer)
      case 102 => intCodeArithmeticOperation(getImmediateParameter)(getPositionParameter)(multiplication)(programState, instructionPointer)
      case 1002 => intCodeArithmeticOperation(getPositionParameter)(getImmediateParameter)(multiplication)(programState, instructionPointer)
      case 1102 => intCodeArithmeticOperation(getImmediateParameter)(getImmediateParameter)(multiplication)(programState, instructionPointer)
      case _ => (List(), instructionPointer)
    }
  }

  def getImmediateParameter(programState: List[Int], index: Int): Int = {
    programState(index)
  }

  def getPositionParameter(programState: List[Int], index: Int): Int = {
    programState(programState(index))
  }

  def intCodeAdd(programState: List[Int], instructionPointer: Int): (List[Int], Int) = {
    def addition(a: Int, b: Int): Int = a + b
    programState(instructionPointer) match {
      case 1 => intCodeArithmeticOperation(getPositionParameter)(getPositionParameter)(addition)(programState, instructionPointer)
      case 101 => intCodeArithmeticOperation(getImmediateParameter)(getPositionParameter)(addition)(programState, instructionPointer)
      case 1001 => intCodeArithmeticOperation(getPositionParameter)(getImmediateParameter)(addition)(programState, instructionPointer)
      case 1101 => intCodeArithmeticOperation(getImmediateParameter)(getImmediateParameter)(addition)(programState, instructionPointer)
      case _ => (List(), instructionPointer)
    }
  }

  def intCodeInput(programState: List[Int], instructionPointer: Int, valToWrite: Int): (List[Int], Int) = {
    (programState.updated(programState(instructionPointer+1), valToWrite), instructionPointer+2)
  }

  def intCodeOutput(programState: List[Int], instructionPointer: Int): (List[Int], Int) = {
    // TODO: use cats effect IO
    programState(instructionPointer) match {
      case 4 => println(getPositionParameter(programState, instructionPointer+1))
      case 104 => println(getImmediateParameter(programState, instructionPointer+1))
    }
    (programState, instructionPointer+2)
  }

  def intCodeBranchOperation(getArg1: (List[Int], Int) => Int)(getArg2: (List[Int], Int) => Int)(operation: (Int) => Boolean)(programState: List[Int], instructionPointer: Int): (List[Int], Int) = {
    val firstOperand = getArg1(programState, instructionPointer+1)

    if (operation(firstOperand)) {
      (programState, getArg2(programState, instructionPointer+2))
    } else {
      (programState, instructionPointer+3)
    }
  }

  def intCodeJumpIfTrue(programState: List[Int], instructionPointer: Int): (List[Int], Int) = {
    def isNotZero(a: Int): Boolean = (a != 0)
    programState(instructionPointer) match {
      case 5 => intCodeBranchOperation(getPositionParameter)(getPositionParameter)(isNotZero)(programState, instructionPointer)
      case 105 => intCodeBranchOperation(getImmediateParameter)(getPositionParameter)(isNotZero)(programState, instructionPointer)
      case 1005 => intCodeBranchOperation(getPositionParameter)(getImmediateParameter)(isNotZero)(programState, instructionPointer)
      case 1105 => intCodeBranchOperation(getImmediateParameter)(getImmediateParameter)(isNotZero)(programState, instructionPointer)
      case _ => (List(), instructionPointer)
    }
  }

  def intCodeJumpIfFalse(programState: List[Int], instructionPointer: Int): (List[Int], Int) = {
    def isZero(a: Int): Boolean = (a == 0)
    programState(instructionPointer) match {
      case 6 => intCodeBranchOperation(getPositionParameter)(getPositionParameter)(isZero)(programState, instructionPointer)
      case 106 => intCodeBranchOperation(getImmediateParameter)(getPositionParameter)(isZero)(programState, instructionPointer)
      case 1006 => intCodeBranchOperation(getPositionParameter)(getImmediateParameter)(isZero)(programState, instructionPointer)
      case 1106 => intCodeBranchOperation(getImmediateParameter)(getImmediateParameter)(isZero)(programState, instructionPointer)
      case _ => (List(), instructionPointer)
    }
  }

  def intCodeLessThan(programState: List[Int], instructionPointer: Int): (List[Int], Int) = {
    def lessThan(a: Int, b:Int): Int = (if (a < b) 1 else 0)
    programState(instructionPointer) match {
      case 7 => intCodeArithmeticOperation(getPositionParameter)(getPositionParameter)(lessThan)(programState, instructionPointer)
      case 107 => intCodeArithmeticOperation(getImmediateParameter)(getPositionParameter)(lessThan)(programState, instructionPointer)
      case 1007 => intCodeArithmeticOperation(getPositionParameter)(getImmediateParameter)(lessThan)(programState, instructionPointer)
      case 1107 => intCodeArithmeticOperation(getImmediateParameter)(getImmediateParameter)(lessThan)(programState, instructionPointer)
      case _ => (List(), instructionPointer)
    }
  }

  def intCodeEquals(programState: List[Int], instructionPointer: Int): (List[Int], Int) = {
    def lessThan(a: Int, b:Int): Int = (if (a == b) 1 else 0)
    programState(instructionPointer) match {
      case 8 => intCodeArithmeticOperation(getPositionParameter)(getPositionParameter)(lessThan)(programState, instructionPointer)
      case 108 => intCodeArithmeticOperation(getImmediateParameter)(getPositionParameter)(lessThan)(programState, instructionPointer)
      case 1008 => intCodeArithmeticOperation(getPositionParameter)(getImmediateParameter)(lessThan)(programState, instructionPointer)
      case 1108 => intCodeArithmeticOperation(getImmediateParameter)(getImmediateParameter)(lessThan)(programState, instructionPointer)
      case _ => (List(), instructionPointer)
    }
  }
}
