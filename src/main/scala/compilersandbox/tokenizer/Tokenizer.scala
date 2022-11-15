package compilersandbox.tokenizer

import compilersandbox.*
import compilersandbox.tokenizer.ParenthesisKind.{Close, Open}

import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.collection.mutable

object Tokenizer {

  def tokenize(input: Seq[Char], previous: Token, tokens: List[Token]): Either[TokenizerFailure, List[Token]] = {

    input.headOption match {
      case Some(character) =>
        character match {
          case '+' | '-'   =>
            previous match {
              case Parenthesis(Close) =>
                tokenize(input.tail, Operator(s"$character"), tokens :+ previous)
              case Parenthesis(Open) | Start =>
                tokenize(input.tail, IncompleteIntegerNumber(s"$character"), tokens :+ previous)
              case _: IncompleteOperator =>
                Left(TokenizerFailure("incomplete operator"))
              case IncompleteIntegerNumber(value) =>
                tokenize(input.tail, Operator(s"$character"), tokens :+ IntegerNumber(value))
              case IncompleteFloatingPointNumber(value) =>
                if (value.last == '.') {
                  Left(TokenizerFailure("unexpected token"))
                }
                else {
                  tokenize(input.tail, Operator(s"$character"), tokens :+ FloatingPointNumber(value))
                }
              case _: Operator | _: IntegerNumber | _: FloatingPointNumber | End =>
                ???
            }
          case '*' | '/' | '^' =>
            previous match {
              case IncompleteIntegerNumber(value) =>
                tokenize(input.tail, Operator(s"$character"), tokens :+ IntegerNumber(value))
              case IncompleteFloatingPointNumber(value) =>
                if (value.last == '.') {
                  Left(TokenizerFailure("unexpected token"))
                }
                else {
                  tokenize(input.tail, Operator(s"$character"), tokens :+ FloatingPointNumber(value))
                }
              case Parenthesis(Close) =>
                tokenize(input.tail, Operator(s"$character"), tokens :+ previous)
              case _: Operator | Start | Parenthesis(Open) | _: FloatingPointNumber | _: IntegerNumber | _: IncompleteOperator | End =>
                ???
            }
          case '(' =>
            previous match {
              case Start | _: Operator | Parenthesis(Open) | Parenthesis(Close) =>
                tokenize(input.tail, Parenthesis(Open), tokens :+ previous)
              case IncompleteIntegerNumber(value) =>
                tokenize(input.tail, Parenthesis(Open), tokens :+ IntegerNumber(value))
              case IncompleteFloatingPointNumber(value) =>
                if (value.last == '.') {
                  Left(TokenizerFailure("unexpected token"))
                }
                else {
                  tokenize(input.tail, Parenthesis(Open), tokens :+ FloatingPointNumber(value))
                }
              case _: FloatingPointNumber | _: IntegerNumber | _: IncompleteOperator | End =>
                ???
            }
          case ')' =>
            previous match {
              case Start | _: Operator | Parenthesis(Open) | _: FloatingPointNumber | _: IntegerNumber | _: IncompleteOperator | End =>
                ???
              case Parenthesis(Close) =>
                tokenize(input.tail, Parenthesis(Close), tokens :+ previous)
              case IncompleteIntegerNumber(value) =>
                tokenize(input.tail, Parenthesis(Close), tokens :+ IntegerNumber(value))
              case IncompleteFloatingPointNumber(value) =>
                if (value.last == '.') {
                  Left(TokenizerFailure("unexpected token"))
                }
                else {
                  tokenize(input.tail, Parenthesis(Close), tokens :+ FloatingPointNumber(value))
                }
            }
          case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
            previous match {
              case Start | Parenthesis(Open) | Parenthesis(Close) | _: Operator =>
                tokenize(input.tail, IncompleteIntegerNumber(s"$character"), tokens :+ previous)
              case IncompleteIntegerNumber(value) =>
                tokenize(input.tail, IncompleteIntegerNumber(s"$value$character"), tokens)
              case IncompleteFloatingPointNumber(value) =>
                tokenize(input.tail, IncompleteFloatingPointNumber(s"$value$character"), tokens)
              case _: IncompleteOperator | _: FloatingPointNumber | _: IntegerNumber | End => 
                ???
            }
          case '.' =>
            previous match {
              case IncompleteIntegerNumber(value) =>
                tokenize(input.tail, IncompleteFloatingPointNumber(s"$value$character"), tokens)
              case _: Operator | _: IncompleteOperator | Start | End | _: Parenthesis | _: FloatingPointNumber | _: IntegerNumber | _: IncompleteFloatingPointNumber =>
                Left(TokenizerFailure("unexpected token"))
            }
          case 's' =>
            previous match {
              case operator @ IncompleteOperator("co") =>
                tokenize(input.tail, Operator(s"${operator.value}$character"), tokens)
              case Start | _: Operator | Parenthesis(Open) | Parenthesis(Close) =>
                tokenize(input.tail, IncompleteOperator(s"$character"), tokens :+ previous)
              case _ =>
                Left(TokenizerFailure("unexpected token"))
            }
          case 'i' =>
            previous match {
              case operator @ IncompleteOperator("s") =>
                  tokenize(input.tail, IncompleteOperator(s"${operator.value}$character"), tokens)
              case _ =>
                Left(TokenizerFailure("unexpected token"))
            }
          case 'n' =>
            previous match {
              case operator @ IncompleteOperator("si") =>
                tokenize(input.tail, Operator(s"${operator.value}$character"), tokens)
              case operator @ IncompleteOperator("ta") =>
                tokenize(input.tail, Operator(s"${operator.value}$character"), tokens)
              case _ =>
                Left(TokenizerFailure("unexpected token"))
            }
          case 'c' =>
            previous match {
              case Start | _: Operator | Parenthesis(Open) | Parenthesis(Close) =>
                tokenize(input.tail, IncompleteOperator(s"$character"), tokens :+ previous)
              case _ =>
                Left(TokenizerFailure("unexpected token"))
            }
          case 'o' =>
            previous match {
              case operator @ IncompleteOperator("c") =>
                tokenize(input.tail, IncompleteOperator(s"${operator.value}$character"), tokens)
              case _ =>
                Left(TokenizerFailure("unexpected token"))
            }
          case 't' =>
            previous match {
              case Start | _: Operator | Parenthesis(Open) | Parenthesis(Close) =>
                tokenize(input.tail, IncompleteOperator(s"$character"), tokens :+ previous)
              case _ =>
                Left(TokenizerFailure("unexpected token"))
            }
          case 'a' =>
            previous match {
              case operator @ IncompleteOperator("t") =>
                tokenize(input.tail, IncompleteOperator(s"${operator.value}$character"), tokens)
              case _ =>
                Left(TokenizerFailure("unexpected token"))
            }
          case _ =>
            Left(TokenizerFailure("unexpected token"))
        }
      case None =>
        previous match {
          case IncompleteIntegerNumber(value) =>
            Right((tokens :+ IntegerNumber(value)) :+ End)
          case IncompleteFloatingPointNumber(value) =>
            Right((tokens :+ FloatingPointNumber(value)) :+ End)
          case _: IncompleteOperator =>
            Left(TokenizerFailure("incomplete operator"))
          case _ =>
            Right((tokens :+ previous) :+ End)
        }

    }
  }
}

sealed trait Token {}


case class Operator(value: String) extends Token

case class IncompleteOperator(value: String) extends Token

case object Start extends Token

case object End extends Token

case class IncompleteIntegerNumber(value: String) extends Token //ich glaub das is ne normale number

case class IncompleteFloatingPointNumber(value: String) extends Token

case class IntegerNumber(value: String) extends Token

case class FloatingPointNumber(value: String) extends Token

case class Parenthesis(value: ParenthesisKind) extends Token

enum ParenthesisKind {
  case Open, Close
}

case class TokenizerFailure(message: String)
