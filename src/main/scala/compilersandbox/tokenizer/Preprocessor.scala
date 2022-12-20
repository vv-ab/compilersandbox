package compilersandbox.tokenizer

import Tokens.{IntegerLiteral, End, DecimalLiteral, Ident, Literal, Parenthesis, ParenthesisKind, Start, Token}
import Tokens.ParenthesisKind.{Close, Open}

import scala.annotation.tailrec

object Preprocessor {

  @tailrec
  def preprocess(input: List[Token], result: List[Token]): List[Token] = {

    input.headOption match {
      case Some(currentToken) =>
        currentToken match {
          case Ident("cos") | Ident("sin") | Ident("tan") | Ident("sqrt") =>
            result.lastOption match {
              case Some(value) =>
                value match {
                  case _: Ident | Start | Parenthesis(Open) =>
                    preprocess(input.tail, result :+ currentToken)
                  case End =>
                    throw IllegalStateException("Should never happen ;-)")
                  case Parenthesis(Close) | _: Literal | _: DecimalLiteral | _: IntegerLiteral =>
                    val unseenOperator = Ident("*")
                    preprocess(input.tail, (result :+ unseenOperator) :+ currentToken)
                }
              case None => ??? // error
            }
          case Ident("pi") =>
            result.lastOption match {
              case Some(value) =>
                value match {
                  case End =>
                    throw IllegalStateException("Should never happen ;-)")
                  case Start | _: Ident | Parenthesis(Open)=>
                    val pi = IntegerLiteral("pi")
                    preprocess(input.tail, result :+ pi)
                  case _: Literal | Parenthesis(Close) | _: DecimalLiteral | _: IntegerLiteral =>
                    val pi = IntegerLiteral("pi")
                    val unseenOperator = Ident("*")
                    preprocess(input.tail, (result :+ unseenOperator) :+ pi)
                }
              case None => ??? // error
            }
          case Ident("e") =>
            result.lastOption match {
              case Some(value) =>
                value match {
                  case End =>
                    throw IllegalStateException("Should never happen ;-)")
                  case Start | _: Ident | Parenthesis(Open) =>
                    val e = IntegerLiteral("e")
                    preprocess(input.tail, result :+ e)
                  case _: Literal | Parenthesis(Close) | _: DecimalLiteral | _: IntegerLiteral =>
                    val e = IntegerLiteral("e")
                    val unseenOperator = Ident("*")
                    preprocess(input.tail, (result :+ unseenOperator) :+ e)
                }
              case None => ??? // error
            }
          case _: Ident | Start | End =>
            preprocess(input.tail, result :+ currentToken)
          case _: Literal | _: DecimalLiteral =>
            result.lastOption match {
              case Some(value) =>
                value match {
                  case _: Ident | Start | Parenthesis(Open) =>
                    preprocess(input.tail, result :+ currentToken)
                  case _: IntegerLiteral | Parenthesis(Close) =>
                    val unseenOperator = Ident("*")
                    preprocess(input.tail, (result :+ unseenOperator) :+ currentToken)
                  case _: Literal | _: DecimalLiteral | End =>
                    throw IllegalStateException("Should never happen ;-)")
                }
              case None => ??? // error
            }
          case Parenthesis(Close) =>
            result.lastOption match {
              case Some(value) =>
                value match {
                  case _: Ident | _: Literal | _: DecimalLiteral | Start | Parenthesis(Close) | Parenthesis(Open) | _: IntegerLiteral =>
                    preprocess(input.tail, result :+ currentToken)
                  case End =>
                    throw IllegalStateException("Should never happen ;-)")
                }
              case None => ???
            }
          case Parenthesis(Open) =>
            result.lastOption match {
              case Some(previousToken) =>
                previousToken match {
                  case _: Ident | Start =>
                    preprocess(input.tail, result :+ currentToken)
                  case _: Literal | _: IntegerLiteral | _: DecimalLiteral | Parenthesis(Close) =>
                    val unseenOperator = Ident("*")
                    preprocess(input.tail, (result :+ unseenOperator) :+ currentToken)
                  case Parenthesis(Open) | End =>
                    ??? // error
                }
              case None => ???
            }
        }
      case None =>
        result
    }
  }
  
}
