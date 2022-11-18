package compilersandbox.tokenizer

import compilersandbox.tokenizer.Tokens.ParenthesisKind

object Tokens {


  sealed trait Token {

    def size(): Int
  }

  case object Start extends Token {

    override def size(): Int = 0
  }

  case object End extends Token {

    override def size(): Int = 0
  }

  case class Ident(value: String) extends Token {

    override def size(): Int = value.length
  }

  case class Literal(value: String) extends Token {

    override def size(): Int = value.length
  }

  case class FloatingPointLiteral(value: String) extends Token {

    override def size(): Int = value.length
  }

  case class Parenthesis(value: ParenthesisKind) extends Token {

    override def size(): Int = 1
  }

  enum ParenthesisKind {
    case Open, Close
  }

}
