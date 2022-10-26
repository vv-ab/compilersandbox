package compilersandbox.model

trait Operator {
  def compute(operandA: Operand, operandB: Operand): Int
}
case object Add extends Operator {
  override def compute(operandA: Operand, operandB: Operand): Int = {
    operandA.value + operandB.value
  }
}
case object Sub extends Operator {
  override def compute(operandA: Operand, operandB: Operand): Int = {
    operandA.value - operandB.value
  }
}
case object Mul extends Operator {
  override def compute(operandA: Operand, operandB: Operand): Int = {
    operandA.value * operandB.value
  }
}
case object Div extends Operator {
  override def compute(operandA: Operand, operandB: Operand): Int = {
    operandA.value / operandB.value
  }
}