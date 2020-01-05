package solution5

sealed trait ParameterMode
case object PositionParameter extends ParameterMode
case object ImmediateParameter extends ParameterMode

object ParameterMode {
  def apply(parameterMode: Char): ParameterMode = {
    parameterMode match {
      case '0' => PositionParameter
      case '1' => ImmediateParameter
    }
  }
}
