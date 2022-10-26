package compilersandbox

@main
def main(): Unit = {
  val input = "9 / 2"
  val (operator, operandA, operandB) = parse(input, None, None, None)
  val result = operator.compute(operandA, operandB)
  println(result)
}

case class Operand(value: Int)
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


def parse(input: Seq[Char], operator: Option[Operator], operandA: Option[Operand], operandB: Option[Operand]): (Operator, Operand, Operand) = {

  input.headOption match {
    case Some(c) =>
      c match {
        case '+' =>
          parse(input.tail, Some(Add), operandA, operandB)
        case '-' =>
          parse(input.tail, Some(Sub), operandA, operandB)
        case '*' =>
          parse(input.tail, Some(Mul), operandA, operandB)
        case '/' =>
          parse(input.tail, Some(Div), operandA, operandB)
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          if (operandA.isEmpty) {
            parse(input.tail, operator, Some(Operand(c.asDigit)), operandB)
          }
          else {
            parse(input.tail, operator, operandA, Some(Operand(c.asDigit)))
          }
        case _ =>
          parse(input.tail, operator, operandA, operandB)
      }
    case None =>
      (operator.get, operandA.get, operandB.get)
  }
}