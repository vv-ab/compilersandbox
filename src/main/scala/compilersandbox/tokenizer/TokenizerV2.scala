package compilersandbox.tokenizer

import compilersandbox.tokenizer.TokenizerV2.ParenthesisKind.{Close, Open}


object TokenizerV2 {

  def tokenize(initialInput: String): Either[TokenizerFailure, List[Token]] = {

    def tokenize(input: Seq[Char], previous: Token, tokens: List[Token]): Either[TokenizerFailure, List[Token]] = {

      def currentLocation(): Location = {
        Location(previous.size() + tokens.map({ token => token.size() }).sum)
      }

      input.headOption match {
        case Some(character) =>
          character match {
            case Digit(digit) =>
              previous match {
                case Start | Parenthesis(Open) | Parenthesis(Close) | _: Ident =>
                  tokenize(input.tail, Literal(s"$digit"), tokens :+ previous)
                case Literal(previousValue) =>
                  tokenize(input.tail, Literal(s"$previousValue$digit"), tokens)
                case FloatingPointLiteral(previousValue) =>
                  tokenize(input.tail, FloatingPointLiteral(s"$previousValue$digit"), tokens)
                case End =>
                  ???
              }
            case Dot(dot) =>
              previous match {
                case Literal(previousValue) =>
                  tokenize(input.tail, FloatingPointLiteral(s"$previousValue$dot"), tokens)
                case Start | _: Ident | _: FloatingPointLiteral | Parenthesis(Open) | Parenthesis(Close) =>
                  ???
                case End =>
                  ???
              }
            case Letter(letter)  =>
              previous match {
                case Start | Ident(Operator(_)) | _: Literal | _: FloatingPointLiteral | Parenthesis(Open) | Parenthesis(Close) =>
                  tokenize(input.tail, Ident(s"$letter"), tokens :+ previous)
                case Ident(previousValue) =>
                  tokenize(input.tail, Ident(s"$previousValue$letter"), tokens)
                case End =>
                  ???
              }
            case '+' | '-' =>
              previous match {
                case Start | _: Ident | Parenthesis(Open) =>
                  tokenize(input.tail, Literal(s"$character"), tokens :+ previous)
                case _: Literal | _: FloatingPointLiteral | Parenthesis(Close) =>
                tokenize(input.tail, Ident(s"$character"), tokens :+ previous)
                case End =>
                  ???
              }
            case '*' | '/' | '^' =>
              previous match {
                case Start | _: FloatingPointLiteral | _: Literal | _: Ident | Parenthesis(Open) | Parenthesis(Close) =>
                  tokenize(input.tail, Ident(s"$character"), tokens :+ previous)
                case End =>
                  ???
              }
            case '(' =>
              previous match {
                case Start | _: Literal | _: FloatingPointLiteral | _: Ident | Parenthesis(Open) | Parenthesis(Close) =>
                  tokenize(input.tail, Parenthesis(Open), tokens :+ previous)
                case End =>
                  ???
              }
            case ')' =>
              previous match {
                case Start | _: Literal | _: FloatingPointLiteral | _: Ident | Parenthesis(Open) | Parenthesis(Close) =>
                  tokenize(input.tail, Parenthesis(Close), tokens :+ previous)
                case End =>
                  ???
              }
          }
        case None =>
          Right((tokens :+ previous) :+ End)
      }

    }
    tokenize(initialInput, Start, List.empty)
  }

  object Digit {
    def unapply(value: Char): Option[Char] = if (value.isDigit) Some(value) else None
  }

  object Operator {
    def unapply(value: String): Option[String] = value match {
      case "+" | "-" | "*" | "/" | "^" =>
        Some(value)
      case _ =>
        None
    }
  }

  object Dot {
    def unapply(value: Char): Option[Char] = if (value == '.') Some(value) else None
  }

  object Letter {
    def unapply(value: Char): Option[Char] = if (value.isLetter) Some(value) else None
  }

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

  case class Location(value: Int)

  case class TokenizerFailure(message: String, input: String, location: Location)

}
