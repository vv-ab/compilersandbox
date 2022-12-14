package compilersandbox.parser

sealed trait Operator {
  def precedence(): Int
}
case object OpenParenthesis extends Operator {

  override def precedence(): Int = 0
}
case object CloseParenthesis extends Operator {

  override def precedence(): Int = 0
}
case object Add extends Operator {

  override def precedence(): Int = 1
}
case object Sub extends Operator {

  override def precedence(): Int = 1
}
case object Mul extends Operator {

  override def precedence(): Int = 2
}
case object Div extends Operator {

  override def precedence(): Int = 2
}
case object Pow extends Operator {

  override def precedence(): Int = 3
}
case object Sin extends Operator {

  override def precedence(): Int = 5
}
case object Cos extends Operator {

  override def precedence(): Int = 5
}
case object Tan extends Operator {

  override def precedence(): Int = 5
}
case object Fac extends Operator {

  override def precedence(): Int = 4
}
case object Sqrt extends Operator {

  override def precedence(): Int = 3
}

case object Round extends Operator {

  override def precedence(): Int = 10
}

case object Flo extends Operator {

  override def precedence(): Int = 10
}

case object Ceil extends Operator {

  override def precedence(): Int = 10
}