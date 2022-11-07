package compilersandbox.parser

sealed trait Node {
  def compute(): Int
}

case class OperatorNode(operator: Operator, left: Node, right: Node) extends Node {
  override def compute(): Int = {
    operator.compute(left.compute(), right.compute())
  }
}

case class OperandNode(operand: Operand) extends Node {
  override def compute(): Int = {
    operand.value
  }
}
