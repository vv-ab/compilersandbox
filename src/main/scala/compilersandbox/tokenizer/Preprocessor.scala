package compilersandbox.tokenizer

import compilersandbox.tokenizer
import compilersandbox.tokenizer.ParenthesisKind.{Close, Open}

object Preprocessor {

  def preprocess(input: List[Token], result: List[Token]): List[Token] = {

    input.headOption match {
      case Some(currentToken) =>
        currentToken match {
          case Operator("cos") | Operator("sin") | Operator("tan") =>
            result.lastOption match {
              case Some(value) =>
                value match {
                  case _: Operator | Start | Parenthesis(Open) =>
                    preprocess(input.tail, result :+ currentToken)
                  case _: IncompleteOperator | _: IncompleteFloatingPointNumber | _: IncompleteIntegerNumber | End =>
                    ??? // should never happen
                  case Parenthesis(Close) | _: IntegerNumber | _: FloatingPointNumber =>
                    val unseenOperator = Operator("*")
                    preprocess(input.tail, (result :+ unseenOperator) :+ currentToken)
                }
            }
          case _: Operator | Start | End =>
            preprocess(input.tail, result :+ currentToken)
          case _: IntegerNumber | _: FloatingPointNumber =>
            result.lastOption match {
              case Some(value) =>
                value match {
                  case _: Operator | Start | Parenthesis(Open) =>
                    preprocess(input.tail, result :+ currentToken)
                  case _: IntegerNumber | _: IncompleteOperator | _: IncompleteFloatingPointNumber | _: IncompleteIntegerNumber | _: FloatingPointNumber | End =>
                    ??? // should never happen
                  case Parenthesis(Close) =>
                    val unseenOperator = Operator("*")
                    preprocess(input.tail, (result :+ unseenOperator) :+ currentToken)
                }
              case None => ??? // error
            }
          case Parenthesis(Close) =>
            result.lastOption match {
              case Some(value) =>
                value match {
                  case _: Operator | _: IntegerNumber | _: FloatingPointNumber | Start | Parenthesis(Close) =>
                    preprocess(input.tail, result :+ currentToken)
                  case End | _: IncompleteIntegerNumber | _: IncompleteOperator | _: IncompleteFloatingPointNumber | Parenthesis(Open) =>
                    ??? // error
                }
              case None => ???
            }
          case Parenthesis(Open) =>
            result.lastOption match {
              case Some(previousToken) =>
                previousToken match {
                  case _: Operator | Start =>
                    preprocess(input.tail, result :+ currentToken)
                  case _: IntegerNumber | _: FloatingPointNumber | Parenthesis(Close) =>
                    val unseenOperator = Operator("*")
                    preprocess(input.tail, (result :+ unseenOperator) :+ currentToken)
                  case Parenthesis(Open) | End | _: IncompleteOperator | _: IncompleteIntegerNumber | _: IncompleteFloatingPointNumber =>
                    ??? // error
                }
              case None => ???
            }
          case _: IncompleteIntegerNumber | _: IncompleteOperator | _: IncompleteFloatingPointNumber =>
            ???
        }
      case None =>
        result
    }
  }
}
