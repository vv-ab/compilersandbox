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
              case _: Number | Parenthesis(Close) =>
                tokenize(input.tail, Operator(s"$character"), tokens :+ previous)
              case Parenthesis(Open) | Start =>
                tokenize(input.tail, Number(s"$character"), tokens :+ previous)
              case _: IncompleteOperator =>
                Left(TokenizerFailure("incomplete operator"))
              case _: Operator =>
                ???
            }
          case '*' | '/' | '^' =>
            previous match {
              case _: Operator | Start | Parenthesis(Open) =>
                ???
              case _: Number | Parenthesis(Close) =>
                tokenize(input.tail, Operator(s"$character"), tokens :+ previous)
            }
          case '(' =>
            previous match {
              case Start | _: Operator | _: Number | Parenthesis(Open) | Parenthesis(Close) =>
                tokenize(input.tail, Parenthesis(Open), tokens :+ previous)
            }
          case ')' =>
            previous match {
              case Start | _: Operator | Parenthesis(Open) =>
                ???
              case _: Number | Parenthesis(Close) =>
                tokenize(input.tail, Parenthesis(Close), tokens :+ previous)
            }
          case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
            previous match {
              case Start | Parenthesis(Open) | Parenthesis(Close) | _: Operator =>
                tokenize(input.tail, Number(s"$character"), tokens :+ previous)
              case Number(value) =>
                tokenize(input.tail, Number(s"$value$character"), tokens)


            }
          case 's' =>
            previous match {
              case operator @ IncompleteOperator("co") =>
                tokenize(input.tail, Operator(s"${operator.value}$character"), tokens)
              case Start | _: Operator | Parenthesis(Open) =>
                tokenize(input.tail, IncompleteOperator(s"$character"), tokens :+ previous)
              case _: Number | Parenthesis(Close) =>
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
              case Start | _: Operator | Parenthesis(Open) =>
                tokenize(input.tail, IncompleteOperator(s"$character"), tokens :+ previous)
              case _: Number | Parenthesis(Close) =>
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
              case Start | _: Operator | Parenthesis(Open) =>
                tokenize(input.tail, IncompleteOperator(s"$character"), tokens :+ previous)
              case _: Number | Parenthesis(Close) =>
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

case class Number(value: String) extends Token

case class Parenthesis(value: ParenthesisKind) extends Token

enum ParenthesisKind {
  case Open, Close
}

case class TokenizerFailure(message: String)
