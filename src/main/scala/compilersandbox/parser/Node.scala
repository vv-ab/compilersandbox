package compilersandbox.parser

sealed trait Node {
  def compute(): Double
}

case class OperatorNode(operator: Operator, left: Node, right: Node) extends Node {
  override def compute(): Double = {
    operator.compute(left.compute(), right.compute())
  }
}

case class OperandNode(operand: Operand) extends Node {
  override def compute(): Double = {
    operand.value
  }
}
