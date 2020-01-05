package solution7

import solution7.IntCodeState.{InputRegs, InstructionPointer, Memory, OutputRegs}

sealed trait IntCodeStatus
case object Running extends IntCodeStatus
case object Paused extends IntCodeStatus
case object Halted extends IntCodeStatus
case object Error extends IntCodeStatus

final case class Registers(input: InputRegs, output: OutputRegs)

final case class IntCodeState(memory: Memory,
                              instructionPointer: InstructionPointer,
                              registers: Registers,
                              status: IntCodeStatus) {
  def getCurrentInstruction: Int = {
    memory(instructionPointer)
  }
}

object IntCodeState {
  type Memory = List[Int]
  type OutputRegs = List[Int]
  type InputRegs = List[Int]
  type InstructionPointer = Int

  def apply(intCodeState: IntCodeState): IntCodeState = {
    IntCodeState(intCodeState.memory,
      intCodeState.instructionPointer,
      intCodeState.registers,
      intCodeState.status
    )
  }

  def apply(intCodeState: IntCodeState, intCodeStatus: IntCodeStatus): IntCodeState = {
    IntCodeState(intCodeState.memory,
      intCodeState.instructionPointer,
      intCodeState.registers,
      intCodeStatus
    )
  }
}
