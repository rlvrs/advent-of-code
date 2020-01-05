package solution5

sealed trait Instruction
case class Add(p1: ParameterMode, p2: ParameterMode, p3: PositionParameter.type) extends Instruction
case class Multiply(p1: ParameterMode, p2: ParameterMode, p3: PositionParameter.type) extends Instruction
case class Input(p1: PositionParameter.type) extends Instruction
case class Output(p1: ParameterMode) extends Instruction
case class JumpIfTrue(p1: ParameterMode, p2: ParameterMode) extends Instruction
case class JumpIfFalse(p1: ParameterMode, p2: ParameterMode) extends Instruction
case class LessThan(p1: ParameterMode, p2: ParameterMode, p3: PositionParameter.type) extends Instruction
case class Equals(p1: ParameterMode, p2: ParameterMode, p3: PositionParameter.type) extends Instruction
case object EndOfProgram extends Instruction

object Instruction {
  private def padOpcode(opcode: Int): Option[String] = {
    val opcodePaddedStr = "%05d".format(opcode)
    if (opcodePaddedStr.length == 5) {
      Some(opcodePaddedStr)
    } else {
      None
    }
  }

  def apply(opcode: Int): Option[Instruction] = {
    padOpcode(opcode).map(_.toSeq) match {
      case Some(Seq('0', '0', '0', '9', '9')) => Some(EndOfProgram)
      case Some(Seq('0', p2, p1, '0', '1')) => Some(Add(ParameterMode(p1), ParameterMode(p2), PositionParameter))
      case Some(Seq('0', p2, p1, '0', '2')) => Some(Multiply(ParameterMode(p1), ParameterMode(p2), PositionParameter))
      case Some(Seq('0', '0', '0', '0', '3')) => Some(Input(PositionParameter))
      case Some(Seq('0', '0', p1, '0', '4')) => Some(Output(ParameterMode(p1)))
      case Some(Seq('0', p2, p1, '0', '5')) => Some(JumpIfTrue(ParameterMode(p1), ParameterMode(p2)))
      case Some(Seq('0', p2, p1, '0', '6')) => Some(JumpIfFalse(ParameterMode(p1), ParameterMode(p2)))
      case Some(Seq('0', p2, p1, '0', '7')) => Some(LessThan(ParameterMode(p1), ParameterMode(p2), PositionParameter))
      case Some(Seq('0', p2, p1, '0', '8')) => Some(Equals(ParameterMode(p1), ParameterMode(p2), PositionParameter))
      case _ => None
    }
  }
}
