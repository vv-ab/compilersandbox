package compilersandbox.tokenizer

import Tokens.{ConstantLiteral, End, DecimalLiteral, Ident, IntegerLiteral, Parenthesis, ParenthesisKind, Start, Token}
import Tokens.ParenthesisKind.{Close, Open}

import scala.annotation.tailrec

object Preprocessor {

  def preprocess(input: List[Token]): List[Token] = {

    @tailrec
    def preprocess(input: List[Token], result: List[Token]): List[Token] = {

      input.headOption match {
        case Some(currentToken) =>
          currentToken match {
            case Ident("cos") | Ident("sin") | Ident("tan") | Ident("sqrt") | Ident("floor") | Ident("ceil") | Ident("round") =>
              result.lastOption match {
                case Some(value) =>
                  value match {
                    case _: Ident | Start | Parenthesis(Open) =>
                      preprocess(input.tail, result :+ currentToken)
                    case End =>
                      throw IllegalStateException("Should never happen ;-)")
                    case Parenthesis(Close) | _: IntegerLiteral | _: DecimalLiteral | _: ConstantLiteral =>
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
                    case Start | _: Ident | Parenthesis(Open) =>
                      val pi = ConstantLiteral("pi")
                      preprocess(input.tail, result :+ pi)
                    case _: IntegerLiteral | Parenthesis(Close) | _: DecimalLiteral | _: ConstantLiteral =>
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
                    case _: IntegerLiteral | Parenthesis(Close) | _: DecimalLiteral | _: ConstantLiteral =>
                      val e = ConstantLiteral("e")
                      val unseenOperator = Ident("*")
                      preprocess(input.tail, (result :+ unseenOperator) :+ e)
                  }
                case None => ??? // error
              }
            case _: Ident | Start | End =>
              preprocess(input.tail, result :+ currentToken)
            case _: IntegerLiteral | _: DecimalLiteral =>
              result.lastOption match {
                case Some(value) =>
                  value match {
                    case _: Ident | Start | Parenthesis(Open) =>
                      preprocess(input.tail, result :+ currentToken)
                    case _: ConstantLiteral | Parenthesis(Close) =>
                      val unseenOperator = Ident("*")
                      preprocess(input.tail, (result :+ unseenOperator) :+ currentToken)
                    case _: IntegerLiteral | _: DecimalLiteral | End =>
                      throw IllegalStateException("Should never happen ;-)")
                  }
                case None => ??? // error
              }
            case Parenthesis(Close) =>
              result.lastOption match {
                case Some(value) =>
                  value match {
                    case _: Ident | _: IntegerLiteral | _: DecimalLiteral | Start | Parenthesis(Close) | Parenthesis(Open) | _: ConstantLiteral =>
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
                    case _: IntegerLiteral | _: ConstantLiteral | _: DecimalLiteral | Parenthesis(Close) =>
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

    preprocess(input, List.empty)
  }
}
