package compilersandbox.tokenizer

import compilersandbox.tokenizer
import compilersandbox.tokenizer.ParenthesisKind.{Close, Open}

object Preprocessor {

  def preprocess(input: List[Token], result: List[Token]): List[Token] = {

    input.headOption match {
      case Some(currentToken) =>
        currentToken match {
          case _: Operator | Start | End =>
            preprocess(input.tail, result :+ currentToken)
          case _: tokenizer.Number =>
            result.lastOption match {
              case Some(value) =>
                value match {
                  case _: Operator | Start | Parenthesis(Open) =>
                    preprocess(input.tail, result :+ currentToken)
                  case _: tokenizer.Number | End =>
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
                  case _: Operator | _: tokenizer.Number | Start | Parenthesis(Close) =>
                    preprocess(input.tail, result :+ currentToken)
                  case End | Parenthesis(Open) =>
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
                  case _: tokenizer.Number | Parenthesis(Close) =>
                    val unseenOperator = Operator("*")
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
