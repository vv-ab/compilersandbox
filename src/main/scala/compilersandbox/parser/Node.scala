package compilersandbox.parser

sealed trait Node {}

case class OperatorNode(operator: Operator, left: Node, right: Node) extends Node {}

case class OperandNode(operand: Operand) extends Node {}
