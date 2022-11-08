package compilersandbox.parser

import compilersandbox.tokenizer
import compilersandbox.tokenizer.{End, Parenthesis, ParenthesisKind, Start, Token}

import scala.collection.mutable

object Parser {

  def parse(input: List[Token], operatorStack: mutable.Stack[Operator], nodeStack: mutable.Stack[Node]): Node = {

    def insertOperator(operator: Operator, operatorStack: mutable.Stack[Operator], nodeStack: mutable.Stack[Node]): Unit = {
      operator match {
        case OpenParenthesis =>
          operatorStack.push(operator)
        case CloseParenthesis =>
          operatorStack.head match {
            case OpenParenthesis =>
              operatorStack.pop()
            case CloseParenthesis => // this case should never happen
              throw IllegalStateException("Encountered an unexpected closing parenthesis!")
            case Add | Sub | Mul | Div =>
              val right = nodeStack.pop()
              val left = nodeStack.pop()
              val operatorNode = OperatorNode(operatorStack.pop(), left, right)
              nodeStack.push(operatorNode)
              insertOperator(operator, operatorStack, nodeStack)
            case Sin =>
              val right = OperandNode(Operand(0))
              val left = nodeStack.pop()
              val operatorNode = OperatorNode(operatorStack.pop(), left, right)
              nodeStack.push(operatorNode)
              insertOperator(operator, operatorStack, nodeStack)
          }
        case Add | Sub | Mul | Div | Pow | Sin =>
          operatorStack.headOption match {
            case Some(head) if head.precedence() >= operator.precedence() =>
              val (right, left) = head match {
                case OpenParenthesis | CloseParenthesis =>
                  throw IllegalStateException("Should never happen ;-)")
                case Add | Sub | Mul | Div | Pow =>
                  (nodeStack.pop(), nodeStack.pop())
                case Sin =>
                  (OperandNode(Operand(0)), nodeStack.pop())
              }
              val operatorNode = OperatorNode(operatorStack.pop(), left, right)
              nodeStack.push(operatorNode)
              insertOperator(operator, operatorStack, nodeStack)
            case _ =>
              operatorStack.push(operator)
          }
      }
    }

    def drainOperatorStack(operatorStack: mutable.Stack[Operator], nodeStack: mutable.Stack[Node]): Node = {
      if (operatorStack.nonEmpty) {
        val operator = operatorStack.pop()
        val (right, left) = operator match {
          case OpenParenthesis | CloseParenthesis => throw IllegalStateException("Should never happen ;-)")
          case Add | Sub | Mul | Div | Pow =>
            (nodeStack.pop(), nodeStack.pop())
          case Sin =>
            (OperandNode(Operand(0)), nodeStack.pop())
        }
        val operatorNode = OperatorNode(operator, left, right)
        nodeStack.push(operatorNode)
        drainOperatorStack(operatorStack, nodeStack)
      }
      else {
        nodeStack.pop()
      }
    }

    input.headOption match {
      case Some(token) =>
        token match {
          case tokenizer.Operator(value) =>
            value match {
              case "+" =>
                insertOperator(Add, operatorStack, nodeStack)
              case "-" =>
                insertOperator(Sub, operatorStack, nodeStack)
              case "*" =>
                insertOperator(Mul, operatorStack, nodeStack)
              case "/" =>
                insertOperator(Div, operatorStack, nodeStack)
              case "^" =>
                insertOperator(Pow, operatorStack, nodeStack)
              case "sin" =>
                insertOperator(Sin, operatorStack, nodeStack)
            }
          case tokenizer.Number(value) =>
            nodeStack.push(OperandNode(Operand(value.toInt)))
          case Parenthesis(value) =>
            value match {
              case ParenthesisKind.Open =>
                insertOperator(OpenParenthesis, operatorStack, nodeStack)
              case ParenthesisKind.Close =>
                insertOperator(CloseParenthesis, operatorStack, nodeStack)
            }
          case Start =>
          case End =>
        }
        parse(input.tail, operatorStack, nodeStack)
      case None =>
        val result = drainOperatorStack(operatorStack, nodeStack)
        result
    }
  }
}
