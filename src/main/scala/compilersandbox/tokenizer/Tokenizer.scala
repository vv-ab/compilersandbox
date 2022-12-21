package compilersandbox.tokenizer

import compilersandbox.tokenizer.Tokens.{DecimalLiteral, End, Ident, Literal, Parenthesis, Start, Token}
import compilersandbox.util.{Failure, Location}
import compilersandbox.tokenizer.Tokens.ParenthesisKind.{Close, Open}

object Tokenizer {

  def tokenize(initialInput: String): Either[List[TokenizerFailure], List[Token]] = {

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
                case DecimalLiteral(previousValue) =>
                  tokenize(input.tail, DecimalLiteral(s"$previousValue$digit"), tokens)
                case End =>
                  throw IllegalStateException("Should never happen ;-)")
              }
            case Dot(dot) =>
              previous match {
                case Literal(previousValue) =>
                  tokenize(input.tail, DecimalLiteral(s"$previousValue$dot"), tokens)
                case Start | _: Ident | _: DecimalLiteral | Parenthesis(Open) | Parenthesis(Close) =>
                  Left(TokenizerFailure("unexpected token", initialInput, currentLocation()))
                case End =>
                  throw IllegalStateException("Should never happen ;-)")
              }
            case Letter(letter) =>
              previous match {
                case Ident("pi") | Ident("sin") | Ident("tan") | Ident("cos") | Ident("e") | Ident("sqrt") =>
                  tokenize(input.tail, Ident(s"$letter"), tokens :+ previous)
                case Start | Ident(Operator(_)) | _: Literal | _: DecimalLiteral | Parenthesis(Open) | Parenthesis(Close) =>
                  tokenize(input.tail, Ident(s"$letter"), tokens :+ previous)
                case Ident(previousValue) =>
                  tokenize(input.tail, Ident(s"$previousValue$letter"), tokens)
                case End =>
                  throw IllegalStateException("Should never happen ;-)")
              }
            case '+' | '-' =>
              previous match {
                case Ident("pi") | Ident("e") | Ident("!") | _: Literal | _: DecimalLiteral | Parenthesis(Close) =>
                  tokenize(input.tail, Ident(s"$character"), tokens :+ previous)
                case Start | _: Ident | Parenthesis(Open) =>
                  tokenize(input.tail, Literal(s"$character"), tokens :+ previous)
                case End =>
                  throw IllegalStateException("Should never happen ;-)")
              }
            case '*' | '/' | '^' | '!' =>
              previous match {
                case Start | _: DecimalLiteral | _: Literal | _: Ident | Parenthesis(Open) | Parenthesis(Close) =>
                  tokenize(input.tail, Ident(s"$character"), tokens :+ previous)
                case End =>
                  throw IllegalStateException("Should never happen ;-)")
              }
            case '(' =>
              previous match {
                case Start | _: Literal | _: DecimalLiteral | _: Ident | Parenthesis(Open) | Parenthesis(Close) =>
                  tokenize(input.tail, Parenthesis(Open), tokens :+ previous)
                case End =>
                  throw IllegalStateException("Should never happen ;-)")
              }
            case ')' =>
              previous match {
                case Start | _: Literal | _: DecimalLiteral | _: Ident | Parenthesis(Open) | Parenthesis(Close) =>
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

    tokenize(initialInput.toLowerCase, Start, List.empty).left.map(List(_))
  }

  object Digit {
    def unapply(value: Char): Option[Char] = if (value.isDigit) Some(value) else None
  }

  object Operator {
    def unapply(value: String): Option[String] = value match {
      case "+" | "-" | "*" | "/" | "^" | "!" =>
        Some(value)
      case _ =>
        None
    }
  }

  object Dot {
    def unapply(value: Char): Option[Char] = if (value == '.' || value == ',') Some(value) else None
  }

  object Letter {
    def unapply(value: Char): Option[Char] = if (value.isLetter) Some(value) else None
  }

  case class TokenizerFailure(message: String, input: String, location: Location) extends Failure

}
