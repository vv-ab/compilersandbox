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

  case class IntegerLiteral(value: String) extends Token {

    override def size(): Int = value.length
  }

  case class DecimalLiteral(value: String) extends Token {

    override def size(): Int = value.length
  }
  
  case class ConstantLiteral(value: String) extends Token {

    override def size(): Int = value.length  
  }
  
  case class Parenthesis(value: ParenthesisKind) extends Token {

    override def size(): Int = 1
  }

  enum ParenthesisKind {
    case Open, Close
  }

}
