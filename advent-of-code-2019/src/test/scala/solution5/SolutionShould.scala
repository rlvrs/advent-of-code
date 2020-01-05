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

  def intCodeArithmeticOperation(operation: (Int, Int) => Int)(getArg1: (List[Int], Int) => Int)(getArg2: (List[Int], Int) => Int)(programState: List[Int], instructionPointer: Int): (List[Int], Int) = {
    val firstOperand = getArg1(programState, instructionPointer+1)
    val secondOperand = getArg2(programState, instructionPointer+2)
    val resultIndex = programState(instructionPointer+3)

    val result = operation(firstOperand, secondOperand)

    (programState.updated(resultIndex, result), instructionPointer+4)
  }

  def intCodeMultiply(programState: List[Int], instructionPointer: Int): (List[Int], Int) = {
    def multiplication(a: Int, b: Int): Int = a * b
    val multiplicationOperation = (intCodeArithmeticOperation _)(multiplication)

    programState(instructionPointer) match {
      case 2 => multiplicationOperation(getPositionParameter)(getPositionParameter)(programState, instructionPointer)
      case 102 => multiplicationOperation(getImmediateParameter)(getPositionParameter)(programState, instructionPointer)
      case 1002 => multiplicationOperation(getPositionParameter)(getImmediateParameter)(programState, instructionPointer)
      case 1102 => multiplicationOperation(getImmediateParameter)(getImmediateParameter)(programState, instructionPointer)
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
    val additionOperation = (intCodeArithmeticOperation _)(addition)

    programState(instructionPointer) match {
      case 1 => additionOperation(getPositionParameter)(getPositionParameter)(programState, instructionPointer)
      case 101 => additionOperation(getImmediateParameter)(getPositionParameter)(programState, instructionPointer)
      case 1001 => additionOperation(getPositionParameter)(getImmediateParameter)(programState, instructionPointer)
      case 1101 => additionOperation(getImmediateParameter)(getImmediateParameter)(programState, instructionPointer)
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

  def intCodeBranchOperation(operation: Int => Boolean)(getArg1: (List[Int], Int) => Int)(getArg2: (List[Int], Int) => Int)(programState: List[Int], instructionPointer: Int): (List[Int], Int) = {
    val firstOperand = getArg1(programState, instructionPointer+1)

    if (operation(firstOperand)) {
      (programState, getArg2(programState, instructionPointer+2))
    } else {
      (programState, instructionPointer+3)
    }
  }

  def intCodeJumpIfTrue(programState: List[Int], instructionPointer: Int): (List[Int], Int) = {
    def isNotZero(a: Int): Boolean = (a != 0)
    val jumpIfNotZero = (intCodeBranchOperation _)(isNotZero)

    programState(instructionPointer) match {
      case 5 => jumpIfNotZero(getPositionParameter)(getPositionParameter)(programState, instructionPointer)
      case 105 => jumpIfNotZero(getImmediateParameter)(getPositionParameter)(programState, instructionPointer)
      case 1005 => jumpIfNotZero(getPositionParameter)(getImmediateParameter)(programState, instructionPointer)
      case 1105 => jumpIfNotZero(getImmediateParameter)(getImmediateParameter)(programState, instructionPointer)
      case _ => (List(), instructionPointer)
    }
  }

  def intCodeJumpIfFalse(programState: List[Int], instructionPointer: Int): (List[Int], Int) = {
    def isZero(a: Int): Boolean = (a == 0)
    val jumpIfIsZero = (intCodeBranchOperation _)(isZero)

    programState(instructionPointer) match {
      case 6 => jumpIfIsZero(getPositionParameter)(getPositionParameter)(programState, instructionPointer)
      case 106 => jumpIfIsZero(getImmediateParameter)(getPositionParameter)(programState, instructionPointer)
      case 1006 => jumpIfIsZero(getPositionParameter)(getImmediateParameter)(programState, instructionPointer)
      case 1106 => jumpIfIsZero(getImmediateParameter)(getImmediateParameter)(programState, instructionPointer)
      case _ => (List(), instructionPointer)
    }
  }

  def intCodeLessThan(programState: List[Int], instructionPointer: Int): (List[Int], Int) = {
    def lessThan(a: Int, b:Int): Int = (if (a < b) 1 else 0)
    val isLessThan = (intCodeArithmeticOperation _)(lessThan)

    programState(instructionPointer) match {
      case 7 => isLessThan(getPositionParameter)(getPositionParameter)(programState, instructionPointer)
      case 107 => isLessThan(getImmediateParameter)(getPositionParameter)(programState, instructionPointer)
      case 1007 => isLessThan(getPositionParameter)(getImmediateParameter)(programState, instructionPointer)
      case 1107 => isLessThan(getImmediateParameter)(getImmediateParameter)(programState, instructionPointer)
      case _ => (List(), instructionPointer)
    }
  }

  def intCodeEquals(programState: List[Int], instructionPointer: Int): (List[Int], Int) = {
    def equals(a: Int, b:Int): Int = (if (a == b) 1 else 0)
    val isEquals = (intCodeArithmeticOperation _)(equals)

    programState(instructionPointer) match {
      case 8 => isEquals(getPositionParameter)(getPositionParameter)(programState, instructionPointer)
      case 108 => isEquals(getImmediateParameter)(getPositionParameter)(programState, instructionPointer)
      case 1008 => isEquals(getPositionParameter)(getImmediateParameter)(programState, instructionPointer)
      case 1108 => isEquals(getImmediateParameter)(getImmediateParameter)(programState, instructionPointer)
      case _ => (List(), instructionPointer)
    }
  }

  // TODO
  //  programState(instructionPointer) match {
  //    case validInstruction => applyParameterModes(programState(instructionPointer))(programState, instructionPointer)
  //    case _ => (List(), instructionPointer)
  //  }
  //
  //  def applyParameterModes(programInstruction: Int,
  //                          partialFn: (List[Int], Int) => Int, (List[Int], Int) => Int]
  //                         ): PartialFunction[(List[Int], Int) => (List[Int], Int)]
  //    partialFn: (PartialFunction[(List[Int], Int) => Int, List[Int], Int => Int, List[Int], Int => (List[Int], Int)]) = {
  //    programInstruction.toString match {
  //      case s"100${_}" => partialFn(getPositionParameter)(getImmediateParameter)
  //      case s"110${_}" => partialFn(getImmediateParameter)(getImmediateParameter)
  //      case s"10${_}" => partialFn(getImmediateParameter)(getPositionParameter)
  //      case "1" | "2" | "3" |
  //      "4" | "5" | "6" | "7" |
  //      "8" => partialFn(getPositionParameter)(getPositionParameter)
  //    }
  //  }
}
