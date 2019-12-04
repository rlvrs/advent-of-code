package solution2

// TODO IntCodeInstruction, IntCodeInstructionAdd, IntCodeInstructionAdd
// TODO binary lookup on currVerb to be faster. start in 50, go left or right on the tree according to result.

object Solution {
  private def withNounVerb(programState: List[Int], noun: Int, verb: Int): List[Int] = {
    programState
      .updated(1, noun)
      .updated(2, verb)
  }

  private def findNounVerbToOutput(programState: List[Int], targetOutput: Int): (Int, Int) = {
    val (lastNoun, lastVerb) = (for {
      currNoun <- List.range(0, 99)
      currVerb <- List.range(0, 99)
    } yield (currNoun, currVerb))
      .dropWhile {
        case (currNoun: Int, currVerb: Int) => executeIntCodeProgram(withNounVerb(programState, currNoun, currVerb)).head != targetOutput
      }.head

    (lastNoun, lastVerb)
  }

  def findComposedNounVerbToOutput(programState: List[Int], targetOutput: Int): Int = {
    val (noun, verb) = findNounVerbToOutput(programState, targetOutput)
    100 * noun + verb
  }

  def executeIntCodeProgram(programState: List[Int]): List[Int] = {
    val instructionLength = 4

    @scala.annotation.tailrec
    def loop(instructionPointer: Int, programState: List[Int]): List[Int] = {
      programState(instructionPointer) match {
        case 99 => programState
        case 1 => loop(instructionPointer + instructionLength, intCodeAdd(instructionPointer, programState))
        case 2 => loop(instructionPointer + instructionLength, intCodeMultiply(instructionPointer, programState))
        case _ => List()
      }
    }

    loop(0, programState)
  }

  def intCodeOperation(instructionPointer: Int, programState: List[Int], operation: (Int, Int) => Int): List[Int] = {
    val firstOperandIndex = programState(instructionPointer+1)
    val secondOperandIndex = programState(instructionPointer+2)
    val resultIndex = programState(instructionPointer+3)

    val result = operation(programState(firstOperandIndex), programState(secondOperandIndex))

    programState.updated(resultIndex, result)
  }

  def intCodeMultiply(instructionPointer: Int, programState: List[Int]): List[Int] = {
    def multiplication(a: Int, b: Int): Int = a * b
    intCodeOperation(instructionPointer, programState, multiplication)
  }

  def intCodeAdd(instructionPointer: Int, programState: List[Int]): List[Int] = {
    def addition(a: Int, b: Int): Int = a + b
    intCodeOperation(instructionPointer, programState, addition)
  }
}