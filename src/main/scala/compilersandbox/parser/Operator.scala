package compilersandbox.parser

sealed trait Operator {
  def compute(left: Double, right: Double): Double
  def precedence(): Int
}
case object OpenParenthesis extends Operator {
  override def compute(left: Double, right: Double): Double = {
    ???
  }

  override def precedence(): Int = 0
}
case object CloseParenthesis extends Operator {
  override def compute(left: Double, right: Double): Double = {
    ???
  }

  override def precedence(): Int = 0
}
case object Add extends Operator {
  override def compute(left: Double, right: Double): Double = {
    left + right
  }

  override def precedence(): Int = 1
}
case object Sub extends Operator {
  override def compute(left: Double, right: Double): Double = {
    left - right
  }

  override def precedence(): Int = 1
}
case object Mul extends Operator {
  override def compute(left: Double, right: Double): Double = {
    left * right
  }

  override def precedence(): Int = 2
}
case object Div extends Operator {
  override def compute(left: Double, right: Double): Double = {
    left / right
  }

  override def precedence(): Int = 2
}

case object Pow extends Operator {
  override def compute(left: Double, right: Double): Double = {
    Math.pow(left, right).asInstanceOf[Int]
  }

  override def precedence(): Int = 3
}

case object Sin extends Operator {
  override def compute(left: Double, right: Double): Double = {
    Math.sin(Math.toRadians(left)).asInstanceOf[Int]
  }

  override def precedence(): Int = 4
}

case object Cos extends Operator {
  override def compute(left: Double, right: Double): Double = {
    Math.cos(Math.toRadians(left)).asInstanceOf[Int]
  }

  override def precedence(): Int = 4
}

case object Tan extends Operator {
  override def compute(left: Double, right: Double): Double = {
    Math.tan(Math.toRadians(left)).asInstanceOf[Int]
  }

  override def precedence(): Int = 4
}
