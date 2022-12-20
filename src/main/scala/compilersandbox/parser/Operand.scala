package compilersandbox.parser

enum DataType {
  case Decimal, Integer
}

sealed trait Operand {
  val dataType: DataType
}

case class DecimalOperand(value: Double) extends Operand {
  override val dataType: DataType = DataType.Decimal
}

case class IntegerOperand(value: Int) extends Operand {
  override val dataType: DataType = DataType.Integer
}
