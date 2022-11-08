package compilersandbox.parser

sealed trait Operator {
  def compute(left: Int, right: Int): Int
  def precedence(): Int
}
case object OpenParenthesis extends Operator {
  override def compute(left: Int, right: Int): Int = {
    ???
  }

  override def precedence(): Int = 0
}
case object CloseParenthesis extends Operator {
  override def compute(left: Int, right: Int): Int = {
    ???
  }

  override def precedence(): Int = 0
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

case object Pow extends Operator {
  override def compute(left: Int, right: Int): Int = {
    Math.pow(left, right).asInstanceOf[Int]
  }

  override def precedence(): Int = 3
}

case object Sin extends Operator {
  override def compute(left: Int, right: Int): Int = {
    Math.sin(Math.toRadians(left)).asInstanceOf[Int]
  }

  override def precedence(): Int = 4
}
