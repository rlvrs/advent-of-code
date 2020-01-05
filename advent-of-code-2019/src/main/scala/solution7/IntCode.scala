package solution7

import solution7.IntCodeState.{InstructionPointer, Memory, OutputRegs}

import scala.annotation.tailrec

// TODO Use the Either pattern for better error handling

object IntCode {
  def executeIntCodeProgram(intCodeState: IntCodeState): IntCodeState = {
    @tailrec
    def cpuTick(intCodeState: IntCodeState): IntCodeState = {
      val newIntCodeState = executeInstruction(intCodeState)

      newIntCodeState.status match {
        case Running => cpuTick(newIntCodeState)
        case _ => newIntCodeState
      }
    }

    cpuTick(intCodeState)
  }

  private def executeInstruction(intCodeState: IntCodeState): IntCodeState = {
    Instruction(intCodeState.getCurrentInstruction) match {
      case Some(EndOfProgram) =>
        IntCodeState(intCodeState, Halted)
      case Some(Add(p1, p2, _)) =>
        val (newMem, newIp) = intCodeAdd(intCodeState.memory, intCodeState.instructionPointer, p1, p2)
        IntCodeState(newMem, newIp, intCodeState.registers, Running)
      case Some(Multiply(p1, p2, _)) =>
        val (newMem, newIp) = intCodeMultiply(intCodeState.memory, intCodeState.instructionPointer, p1, p2)
        IntCodeState(newMem, newIp, intCodeState.registers, Running)
      case Some(Input(_)) =>
        intCodeState.registers.input match {
          case Nil => IntCodeState(intCodeState, Paused)
          case head :: tail =>
            val (newMem, newIp) = intCodeInput(intCodeState.memory,
              intCodeState.instructionPointer,
              head)
            IntCodeState(newMem, newIp, Registers(tail, intCodeState.registers.output), Running)
        }
      case Some(Output(p1)) =>
        val (newMem, newIp, newOuts) = intCodeOutput(intCodeState.memory, intCodeState.instructionPointer, p1, intCodeState.registers.output)
        IntCodeState(newMem, newIp, Registers(intCodeState.registers.input, newOuts), Running)
      case Some(JumpIfTrue(p1, p2)) =>
        val (newMem, newIp) = intCodeJumpIfTrue(intCodeState.memory, intCodeState.instructionPointer, p1, p2)
        IntCodeState(newMem, newIp, intCodeState.registers, Running)
      case Some(JumpIfFalse(p1, p2)) =>
        val (newMem, newIp) = intCodeJumpIfFalse(intCodeState.memory, intCodeState.instructionPointer, p1, p2)
        IntCodeState(newMem, newIp, intCodeState.registers, Running)
      case Some(LessThan(p1, p2, _)) =>
        val (newMem, newIp) = intCodeLessThan(intCodeState.memory, intCodeState.instructionPointer, p1, p2)
        IntCodeState(newMem, newIp, intCodeState.registers, Running)
      case Some(Equals(p1, p2, _)) =>
        val (newMem, newIp) = intCodeEquals(intCodeState.memory, intCodeState.instructionPointer, p1, p2)
        IntCodeState(newMem, newIp, intCodeState.registers, Running)
      case _ =>
        IntCodeState(intCodeState, Error)
    }
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
