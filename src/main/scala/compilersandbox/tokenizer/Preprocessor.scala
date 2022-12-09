package compilersandbox.tokenizer

import Tokens.{ConstantLiteral, End, FloatingPointLiteral, Ident, Literal, Parenthesis, ParenthesisKind, Start, Token}
import Tokens.ParenthesisKind.{Close, Open}

object Preprocessor {

  def preprocess(input: List[Token], result: List[Token]): List[Token] = {

    input.headOption match {
      case Some(currentToken) =>
        currentToken match {
          case Ident("cos") | Ident("sin") | Ident("tan") =>
            result.lastOption match {
              case Some(value) =>
                value match {
                  case _: Ident | Start | Parenthesis(Open) =>
                    preprocess(input.tail, result :+ currentToken)
                  case End =>
                    throw IllegalStateException("Should never happen ;-)")
                  case Parenthesis(Close) | _: Literal | _: FloatingPointLiteral =>
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
                    val pi = ConstantLiteral("pi")
                    preprocess(input.tail, result :+ pi)
                  case _: Literal | Parenthesis(Close) | _: FloatingPointLiteral | _: ConstantLiteral =>
                    val pi = ConstantLiteral("pi")
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
                    val e = ConstantLiteral("e")
                    preprocess(input.tail, result :+ e)
                  case _: Literal | Parenthesis(Close) | _: FloatingPointLiteral | _: ConstantLiteral =>
                    val e = ConstantLiteral("e")
                    val unseenOperator = Ident("*")
                    preprocess(input.tail, (result :+ unseenOperator) :+ e)
                }
              case None => ??? // error
            }
          case _: Ident | Start | End =>
            preprocess(input.tail, result :+ currentToken)
          case _: Literal | _: FloatingPointLiteral =>
            result.lastOption match {
              case Some(value) =>
                value match {
                  case _: Ident | Start | Parenthesis(Open) =>
                    preprocess(input.tail, result :+ currentToken)
                  case FloatingPointLiteral("pi") | Parenthesis(Close) =>
                    val unseenOperator = Ident("*")
                    preprocess(input.tail, (result :+ unseenOperator) :+ currentToken)
                  case _: Literal | _: FloatingPointLiteral | End =>
                    throw IllegalStateException("Should never happen ;-)")
                }
              case None => ??? // error
            }
          case Parenthesis(Close) =>
            result.lastOption match {
              case Some(value) =>
                value match {
                  case _: Ident | _: Literal | _: FloatingPointLiteral | Start | Parenthesis(Close) | Parenthesis(Open) =>
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
                  case _: Literal | _: FloatingPointLiteral | Parenthesis(Close) =>
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
