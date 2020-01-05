package solution5

// TODO Use the Either pattern for better error handling

object Solution {
  type Memory = List[Int]
  type OutputRegs = List[Int]
  type InputRegs = List[Int]
  type InstructionPointer = Int

  def executeIntCodeProgram(memory: Memory, inputRegs: InputRegs): (Memory, OutputRegs) = {
    @scala.annotation.tailrec
    def loop(memory: Memory, instructionPointer: InstructionPointer, inputRegs: InputRegs, outputRegs: OutputRegs): (Memory, OutputRegs) = {
      import shapeless.syntax.std.tuple._

      val instructionCode = memory(instructionPointer)
      val (newProgramState, newInstructionPointer, newOutputs) = Instruction(instructionCode) match {
        case Some(EndOfProgram) => (memory, -1, outputRegs)
        case Some(Add(p1, p2, _)) => intCodeAdd(memory, instructionPointer, p1, p2) ::: Tuple1(outputRegs)
        case Some(Multiply(p1, p2, _)) => intCodeMultiply(memory, instructionPointer, p1, p2) ::: Tuple1(outputRegs)
        case Some(Input(_)) => intCodeInput(memory, instructionPointer, inputRegs.head) ::: Tuple1(outputRegs)
        case Some(Output(p1)) => intCodeOutput(memory, instructionPointer, p1, outputRegs)
        case Some(JumpIfTrue(p1, p2)) => intCodeJumpIfTrue(memory, instructionPointer, p1, p2) ::: Tuple1(outputRegs)
        case Some(JumpIfFalse(p1, p2)) => intCodeJumpIfFalse(memory, instructionPointer, p1, p2) ::: Tuple1(outputRegs)
        case Some(LessThan(p1, p2, _)) => intCodeLessThan(memory, instructionPointer, p1, p2) ::: Tuple1(outputRegs)
        case Some(Equals(p1, p2, _)) => intCodeEquals(memory, instructionPointer, p1, p2) ::: Tuple1(outputRegs)
        case _ => (List(), -1, List())
      }

      if (newInstructionPointer < 0) {
        (newProgramState, newOutputs)
      } else if (isInputInstruction(instructionCode)) {
        loop(newProgramState, newInstructionPointer, inputRegs.tail, newOutputs)
      } else {
        loop(newProgramState, newInstructionPointer, inputRegs, newOutputs)
      }
    }

    loop(memory, 0, inputRegs, List())
  }

  private def isInputInstruction(instructionCode: Int): Boolean = {
    instructionCode == 3
  }

  def intCodeArithmeticOperation(operation: (Int, Int) => Int)(getArg1: (Memory, Int) => Int)(getArg2: (Memory, Int) => Int)(memory: Memory, instructionPointer: InstructionPointer): (Memory, InstructionPointer) = {
    val firstOperand = getArg1(memory, instructionPointer+1)
    val secondOperand = getArg2(memory, instructionPointer+2)
    val resultIndex = memory(instructionPointer+3)

    val result = operation(firstOperand, secondOperand)

    (memory.updated(resultIndex, result), instructionPointer+4)
  }

  def getImmediateParameter(memory: Memory, index: Int): Int = {
    memory(index)
  }

  def getPositionParameter(memory: Memory, index: Int): Int = {
    memory(memory(index))
  }

  private def locateParameterGetter(parameterMode: ParameterMode): (Memory, Int) => Int = {
    parameterMode match {
      case PositionParameter => getPositionParameter
      case ImmediateParameter => getImmediateParameter
    }
  }

  def intCodeMultiply(memory: Memory, instructionPointer: InstructionPointer, p1: ParameterMode, p2: ParameterMode): (Memory, InstructionPointer) = {
    def multiplication(a: Int, b: Int): Int = a * b
    val multiplicationOperation = (intCodeArithmeticOperation _)(multiplication)

    multiplicationOperation(locateParameterGetter(p1))(locateParameterGetter(p2))(memory, instructionPointer)
  }

  def intCodeAdd(memory: Memory, instructionPointer: InstructionPointer, p1: ParameterMode, p2: ParameterMode): (Memory, InstructionPointer) = {
    def addition(a: Int, b: Int): Int = a + b
    val additionOperation = (intCodeArithmeticOperation _)(addition)

    additionOperation(locateParameterGetter(p1))(locateParameterGetter(p2))(memory, instructionPointer)
  }

  def intCodeInput(memory: Memory, instructionPointer: InstructionPointer, valToWrite: Int): (Memory, InstructionPointer) = {
    (memory.updated(memory(instructionPointer+1), valToWrite), instructionPointer+2)
  }

  def intCodeOutput(memory: Memory, instructionPointer: InstructionPointer, p1: ParameterMode, outputRegs: OutputRegs): (Memory, InstructionPointer, OutputRegs) = {
    val outputVal = locateParameterGetter(p1)(memory, instructionPointer+1)

    // TODO: use cats effect IO
    println(outputVal)
    (memory, instructionPointer+2, outputRegs ::: List(outputVal))
  }

  def intCodeBranchOperation(operation: Int => Boolean)(getArg1: (Memory, Int) => Int)(getArg2: (Memory, Int) => Int)(memory: Memory, instructionPointer: InstructionPointer): (Memory, InstructionPointer) = {
    val firstOperand = getArg1(memory, instructionPointer+1)

    if (operation(firstOperand)) {
      (memory, getArg2(memory, instructionPointer+2))
    } else {
      (memory, instructionPointer+3)
    }
  }

  def intCodeJumpIfTrue(memory: Memory, instructionPointer: InstructionPointer, p1: ParameterMode, p2: ParameterMode): (Memory, InstructionPointer) = {
    def isNotZero(a: Int): Boolean = a != 0
    val jumpIfNotZero = (intCodeBranchOperation _)(isNotZero)

    jumpIfNotZero(locateParameterGetter(p1))(locateParameterGetter(p2))(memory, instructionPointer)
  }

  def intCodeJumpIfFalse(memory: Memory, instructionPointer: InstructionPointer, p1: ParameterMode, p2: ParameterMode): (Memory, InstructionPointer) = {
    def isZero(a: Int): Boolean = a == 0
    val jumpIfIsZero = (intCodeBranchOperation _)(isZero)

    jumpIfIsZero(locateParameterGetter(p1))(locateParameterGetter(p2))(memory, instructionPointer)
  }

  def intCodeLessThan(memory: Memory, instructionPointer: InstructionPointer, p1: ParameterMode, p2: ParameterMode): (Memory, InstructionPointer) = {
    def lessThan(a: Int, b:Int): Int = if (a < b) 1 else 0
    val isLessThan = (intCodeArithmeticOperation _)(lessThan)

    isLessThan(locateParameterGetter(p1))(locateParameterGetter(p2))(memory, instructionPointer)
  }

  def intCodeEquals(memory: Memory, instructionPointer: InstructionPointer, p1: ParameterMode, p2: ParameterMode): (Memory, InstructionPointer) = {
    def equals(a: Int, b:Int): Int = if (a == b) 1 else 0
    val isEquals = (intCodeArithmeticOperation _)(equals)

    isEquals(locateParameterGetter(p1))(locateParameterGetter(p2))(memory, instructionPointer)
  }
}
