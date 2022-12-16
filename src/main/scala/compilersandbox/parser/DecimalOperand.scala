package compilersandbox.parser

sealed trait Operand
case class DecimalOperand(value: Double) extends Operand

case class IntegerOperand(value: Int) extends Operand
