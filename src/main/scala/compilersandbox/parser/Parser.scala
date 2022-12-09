package compilersandbox.parser

import compilersandbox.tokenizer
import compilersandbox.tokenizer.Tokens
import compilersandbox.tokenizer.Tokens.{ConstantLiteral, End, FloatingPointLiteral, Ident, Literal, Parenthesis, ParenthesisKind, Start, Token}
import compilersandbox.util.Location

import scala.annotation.tailrec
import scala.collection.mutable

object Parser {

  def parse(initialInput: List[Token]): Either[ParsingFailure, Node] = {

    @tailrec
    def parse(input: List[Token], operatorStack: mutable.Stack[Operator], nodeStack: mutable.Stack[Node]): Either[ParsingFailure, Node] = {

      def currentLocation(): Location = {
        Location(initialInput.size - input.size)
      }

      def makeUnaryOperatorNode(operatorStack: mutable.Stack[Operator], nodeStack: mutable.Stack[Node]): Either[ParsingFailure, Node] = {
        if (nodeStack.nonEmpty) {
          val right = OperandNode(Operand(0))
          val left = nodeStack.pop()
          Right(OperatorNode(operatorStack.pop(), left, right))
        }
        else {
          Left(ParsingFailure("missing operand", initialInput, currentLocation()))
        }
      }

      def makeBinaryOperatorNode(operatorStack: mutable.Stack[Operator], nodeStack: mutable.Stack[Node]): Either[ParsingFailure, Node] = {
        if (nodeStack.size >= 2) {
          val right = nodeStack.pop()
          val left = nodeStack.pop()
          Right(OperatorNode(operatorStack.pop(), left, right))
        }
        else {
          Left(ParsingFailure("missing operand", initialInput, currentLocation()))
        }
      }

      def insertOperator(operator: Operator, operatorStack: mutable.Stack[Operator], nodeStack: mutable.Stack[Node]): Either[ParsingFailure, Unit] = {
        operator match {
          case OpenParenthesis =>
            operatorStack.push(operator)
            Right(())
          case CloseParenthesis =>
            operatorStack.head match {
              case OpenParenthesis =>
                operatorStack.pop()
                Right(())
              case CloseParenthesis =>
                throw IllegalStateException("Encountered an unexpected closing parenthesis!")
              case Add | Sub | Mul | Div | Pow =>
                makeBinaryOperatorNode(operatorStack, nodeStack) match {
                  case Right(node) =>
                    nodeStack.push(node)
                    insertOperator(operator, operatorStack, nodeStack)
                  case Left(failure) =>
                    Left(failure)
                }
              case Sin | Cos | Tan =>
                val maybeNode = makeUnaryOperatorNode(operatorStack, nodeStack)
                maybeNode match {
                  case Right(node) =>
                    nodeStack.push(node)
                    insertOperator(operator, operatorStack, nodeStack)
                  case Left(failure) =>
                    Left(failure)
                }
            }
          case Add | Sub | Mul | Div | Pow | Sin | Cos | Tan =>
            operatorStack.headOption match {
              case Some(head) if head.precedence() >= operator.precedence() =>
                val node: Either[ParsingFailure, Node] = head match {
                  case OpenParenthesis | CloseParenthesis =>
                    Left(ParsingFailure("", initialInput, currentLocation())) // throw IllegalStateException("Should never happen ;-)")
                  case Add | Sub | Mul | Div | Pow =>
                    makeBinaryOperatorNode(operatorStack, nodeStack)
                  case Sin | Cos | Tan =>
                    makeUnaryOperatorNode(operatorStack, nodeStack)
                }
                node match {
                  case Right(node) =>
                    nodeStack.push(node)
                    insertOperator(operator, operatorStack, nodeStack)
                  case Left(failure) =>
                    Left(failure)
                }
              case _ =>
                operatorStack.push(operator)
                Right(())
            }
        }
      }

      def drainOperatorStack(operatorStack: mutable.Stack[Operator], nodeStack: mutable.Stack[Node]): Either[ParsingFailure, Node] = {
        if (operatorStack.nonEmpty) {
          val node = operatorStack.head match {
            case OpenParenthesis | CloseParenthesis =>
              Left(ParsingFailure("", initialInput, currentLocation()))
            // throw IllegalStateException("Should never happen ;-)")
            case Add | Sub | Mul | Div | Pow =>
              makeBinaryOperatorNode(operatorStack, nodeStack)
            case Sin | Cos | Tan =>
              makeUnaryOperatorNode(operatorStack, nodeStack)
          }
          node match {
            case Right(node) =>
              nodeStack.push(node)
              drainOperatorStack(operatorStack, nodeStack)
            case Left(failure) =>
              Left(failure)
          }
        }
        else {
          if (nodeStack.size != 1) {
            Left(ParsingFailure("", initialInput, currentLocation()))
          }
          else {
            Right(nodeStack.pop())
          }
        }
      }

      input.headOption match {
        case Some(token) =>
          token match {
            case Ident(value) =>
              val result = value match {
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
                case "cos" =>
                  insertOperator(Cos, operatorStack, nodeStack)
                case "tan" =>
                  insertOperator(Tan, operatorStack, nodeStack)
                case _ =>
                  Left(ParsingFailure(s"unknown token: $value", initialInput, currentLocation()))
              }
              result match {
                case Left(failure) =>
                  Left(failure)
                case _ =>
                  parse(input.tail, operatorStack, nodeStack)
              }
            case Literal(value) =>
              value.toDoubleOption match {
                case Some(double) =>
                  nodeStack.push(OperandNode(Operand(double)))
                  parse(input.tail, operatorStack, nodeStack)
                case None =>
                  Left(ParsingFailure(s"Literal is not a number: $value", initialInput, currentLocation()))
              }
            case FloatingPointLiteral(value) =>
                  nodeStack.push(OperandNode(Operand(value.toDouble)))
                  parse(input.tail, operatorStack, nodeStack)
            case ConstantLiteral(value) =>
              value match {
                case "pi" =>
                  nodeStack.push(OperandNode(Operand(Math.PI)))
                  parse(input.tail, operatorStack, nodeStack)
                case "e" =>
                  nodeStack.push(OperandNode(Operand(Math.E)))
                  parse(input.tail, operatorStack, nodeStack)
                case _ =>
                  ???
              }
            case Parenthesis(value) =>
              value match {
                case ParenthesisKind.Open =>
                  insertOperator(OpenParenthesis, operatorStack, nodeStack)
                case ParenthesisKind.Close =>
                  insertOperator(CloseParenthesis, operatorStack, nodeStack)
              } match {
                case Left(failure) =>
                  Left(failure)
                case _ =>
                  parse(input.tail, operatorStack, nodeStack)
              }
            case Start | End =>
              parse(input.tail, operatorStack, nodeStack)
          }
        case None =>
          drainOperatorStack(operatorStack, nodeStack)
      }

    }

    parse(initialInput, mutable.Stack.empty, mutable.Stack.empty)
  }
}

case class ParsingFailure(message: String, input: List[Token], location: Location)
