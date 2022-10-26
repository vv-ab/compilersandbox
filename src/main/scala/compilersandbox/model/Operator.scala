package compilersandbox.model

trait Operator {
  def compute(left: Int, right: Int): Int
}
case object Add extends Operator {
  override def compute(left: Int, right: Int): Int = {
    left + right
  }
}
case object Sub extends Operator {
  override def compute(left: Int, right: Int): Int = {
    left - right
  }
}
case object Mul extends Operator {
  override def compute(left: Int, right: Int): Int = {
    left * right
  }
}
case object Div extends Operator {
  override def compute(left: Int, right: Int): Int = {
    left / right
  }
}