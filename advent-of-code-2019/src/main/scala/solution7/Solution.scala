package solution7

import solution7.IntCodeState.Memory

import scala.annotation.tailrec

object Solution {
  def maximizeThrustersInputPart1(memory: Memory, phases: Range): Int = {
    @tailrec
    def calculateAmplifiersOutput(phasesPermutation: List[Int], input: Int): Int = {
      phasesPermutation match {
        case Nil => input
        case head :: tail =>
          val newState = IntCode.executeIntCodeProgram(
            IntCodeState(memory, 0, Registers(List(head, input), List()), Paused)
          )
          calculateAmplifiersOutput(tail, newState.registers.output.last)
      }
    }

    phases.toList.permutations
      .map(phases => calculateAmplifiersOutput(phases, 0))
      .max
  }

  def maximizeThrustersInputPart2(memory: Memory, phases: Range): Int = {
    @tailrec
    def calculateAmplifiersOutput(amplifiersState: List[IntCodeState],
                                  input: Int,
                                  iterator: Iterator[Int]): Int = {
      amplifiersState.last.status match {
        case Halted => amplifiersState.last.registers.output.last
        case _ =>
          val index = iterator.next
          val amplifierState = amplifiersState(index)

          val newState: IntCodeState = IntCode.executeIntCodeProgram(
            IntCodeState(
              amplifierState.memory,
              amplifierState.instructionPointer,
              Registers(amplifierState.registers.input ::: List(input),
                amplifierState.registers.output),
              Paused
            )
          )

          calculateAmplifiersOutput(
            amplifiersState.updated(index, newState),
            newState.registers.output.last,
            iterator
          )
      }
    }

    phases.toList.permutations
      .map(phasesPermutation =>
        calculateAmplifiersOutput(
          phasesPermutation.map(phase => IntCodeState(memory, 0, Registers(List(phase), List()), Paused)),
          0,
          Iterator.continually(phases.indices).flatten
        )
      ).max
  }
}
