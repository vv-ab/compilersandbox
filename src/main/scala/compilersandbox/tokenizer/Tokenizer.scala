package compilersandbox.tokenizer

import compilersandbox.tokenizer.Tokens.{End, FloatingPointLiteral, Ident, Literal, Parenthesis, Start, Token}
import compilersandbox.util.Location
import compilersandbox.tokenizer.Tokens.ParenthesisKind.{Open, Close}

object Tokenizer {

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
                  throw IllegalStateException("Should never happen ;-)")
              }
            case Dot(dot) =>
              previous match {
                case Literal(previousValue) =>
                  tokenize(input.tail, FloatingPointLiteral(s"$previousValue$dot"), tokens)
                case Start | _: Ident | _: FloatingPointLiteral | Parenthesis(Open) | Parenthesis(Close) =>
                  Left(TokenizerFailure("unexpected token", initialInput, currentLocation()))
                case End =>
                  throw IllegalStateException("Should never happen ;-)")
              }
            case Letter(letter) =>
              previous match {
                case Start | Ident(Operator(_)) | _: Literal | _: FloatingPointLiteral | Parenthesis(Open) | Parenthesis(Close) =>
                  tokenize(input.tail, Ident(s"$letter"), tokens :+ previous)
                case Ident(previousValue) =>
                  tokenize(input.tail, Ident(s"$previousValue$letter"), tokens)
                case End =>
                  throw IllegalStateException("Should never happen ;-)")
              }
            case '+' | '-' =>
              previous match {
                case Start | _: Ident | Parenthesis(Open) =>
                  tokenize(input.tail, Literal(s"$character"), tokens :+ previous)
                case _: Literal | _: FloatingPointLiteral | Parenthesis(Close) =>
                  tokenize(input.tail, Ident(s"$character"), tokens :+ previous)
                case End =>
                  throw IllegalStateException("Should never happen ;-)")
              }
            case '*' | '/' | '^' =>
              previous match {
                case Start | _: FloatingPointLiteral | _: Literal | _: Ident | Parenthesis(Open) | Parenthesis(Close) =>
                  tokenize(input.tail, Ident(s"$character"), tokens :+ previous)
                case End =>
                  throw IllegalStateException("Should never happen ;-)")
              }
            case '(' =>
              previous match {
                case Start | _: Literal | _: FloatingPointLiteral | _: Ident | Parenthesis(Open) | Parenthesis(Close) =>
                  tokenize(input.tail, Parenthesis(Open), tokens :+ previous)
                case End =>
                  throw IllegalStateException("Should never happen ;-)")
              }
            case ')' =>
              previous match {
                case Start | _: Literal | _: FloatingPointLiteral | _: Ident | Parenthesis(Open) | Parenthesis(Close) =>
                  tokenize(input.tail, Parenthesis(Close), tokens :+ previous)
                case End =>
                  throw IllegalStateException("Should never happen ;-)")
              }
             case ' ' =>
               previous match {
                 case End =>
                   throw IllegalStateException("Should never happen ;-)")
                 case _ =>
                 tokenize(input.tail, previous, tokens)
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

  case class TokenizerFailure(message: String, input: String, location: Location)

}
