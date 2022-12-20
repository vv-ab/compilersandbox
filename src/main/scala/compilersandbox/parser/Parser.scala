package compilersandbox.parser

import compilersandbox.tokenizer
import compilersandbox.tokenizer.Tokens
import compilersandbox.tokenizer.Tokens.{IntegerLiteral, End, DecimalLiteral, Ident, Literal, Parenthesis, ParenthesisKind, Start, Token}
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
          val right = OperandNode(DecimalOperand(0))
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

      @tailrec
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
              case Sin | Cos | Tan | Sqrt=>
                val maybeNode = makeUnaryOperatorNode(operatorStack, nodeStack)
                maybeNode match {
                  case Right(node) =>
                    nodeStack.push(node)
                    insertOperator(operator, operatorStack, nodeStack)
                  case Left(failure) =>
                    Left(failure)
                }
              case Fac =>
                Left(ParsingFailure("Illegal token", initialInput, currentLocation()))
            }
          case Add | Sub | Mul | Div | Pow | Sin | Cos | Tan | Sqrt =>
            operatorStack.headOption match {
              case Some(head) if head.precedence() >= operator.precedence() =>
                val node: Either[ParsingFailure, Node] = head match {
                  case OpenParenthesis | CloseParenthesis | Fac =>
                    Left(ParsingFailure("Illegal token", initialInput, currentLocation()))
                  case Add | Sub | Mul | Div | Pow =>
                    makeBinaryOperatorNode(operatorStack, nodeStack)
                  case Sin | Cos | Tan | Sqrt=>
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

      @tailrec
      def drainOperatorStack(operatorStack: mutable.Stack[Operator], nodeStack: mutable.Stack[Node]): Either[ParsingFailure, Node] = {
        if (operatorStack.nonEmpty) {
          val node = operatorStack.head match {
            case OpenParenthesis | CloseParenthesis | Fac =>
              Left(ParsingFailure("Illegal token", initialInput, currentLocation()))
            case Add | Sub | Mul | Div | Pow =>
              makeBinaryOperatorNode(operatorStack, nodeStack)
            case Sin | Cos | Tan | Sqrt =>
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
            Left(ParsingFailure("could not construct abstract syntax tree", initialInput, currentLocation()))
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
              val result: Either[ParsingFailure, Unit] = value match {
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
                case "!" =>
                  operatorStack.headOption match {
                    case Some(head) if head.precedence() >= Fac.precedence() =>
                      val node: Either[ParsingFailure, Node] = head match {
                        case OpenParenthesis | CloseParenthesis =>
                          Left(ParsingFailure("Illegal token", initialInput, currentLocation()))
                        case Add | Sub | Mul | Div | Pow =>
                          makeBinaryOperatorNode(operatorStack, nodeStack)
                        case Sin | Cos | Tan | Sqrt =>
                          makeUnaryOperatorNode(operatorStack, nodeStack)
                      }
                      node.map({ value =>
                        val right = OperandNode(DecimalOperand(0))
                        nodeStack.push(OperatorNode(Fac, value, right))
                        Right(OperatorNode(Fac, value, right))
                      })
                    case _ =>
                      val right = OperandNode(DecimalOperand(0))
                      val left = nodeStack.pop()
                      nodeStack.push(OperatorNode(Fac, left, right))
                      Right(OperatorNode(Fac, left, right))
                  }
                case "sin" =>
                  insertOperator(Sin, operatorStack, nodeStack)
                case "cos" =>
                  insertOperator(Cos, operatorStack, nodeStack)
                case "tan" =>
                  insertOperator(Tan, operatorStack, nodeStack)
                case "sqrt" =>
                  insertOperator(Sqrt, operatorStack, nodeStack)
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
              value.toIntOption match {
                case Some(value) =>
                  nodeStack.push(OperandNode(IntegerOperand(value)))
                  parse(input.tail, operatorStack, nodeStack)
                case None =>
                  Left(ParsingFailure(s"Literal is not a integer number: $value", initialInput, currentLocation()))
              }
            case DecimalLiteral(value) =>
              value.toDoubleOption match {
                case Some(value) =>
                  nodeStack.push(OperandNode(DecimalOperand(value)))
                  parse(input.tail, operatorStack, nodeStack)
                case None =>
                  Left(ParsingFailure(s"Literal is not a decimal number: $value", initialInput, currentLocation()))
              }
            case IntegerLiteral(value) =>
              value match {
                case "pi" =>
                  nodeStack.push(OperandNode(DecimalOperand(Math.PI)))
                  parse(input.tail, operatorStack, nodeStack)
                case "e" =>
                  nodeStack.push(OperandNode(DecimalOperand(Math.E)))
                  parse(input.tail, operatorStack, nodeStack)
                case _ =>
                  Left(ParsingFailure(s"unknown constant: $value", initialInput, currentLocation()))
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
