package compilersandbox.model

trait Operator {
  def compute(left: Int, right: Int): Int
  def precedence(): Int
}
case object Add extends Operator {
  override def compute(left: Int, right: Int): Int = {
    left + right
  }

  override def precedence(): Int = 1
}
case object Sub extends Operator {
  override def compute(left: Int, right: Int): Int = {
    left - right
  }

  override def precedence(): Int = 1
}
case object Mul extends Operator {
  override def compute(left: Int, right: Int): Int = {
    left * right
  }

  override def precedence(): Int = 2
}
case object Div extends Operator {
  override def compute(left: Int, right: Int): Int = {
    left / right
  }

  override def precedence(): Int = 2
}