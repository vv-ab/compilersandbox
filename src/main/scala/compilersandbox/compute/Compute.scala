package compilersandbox.compute

import compilersandbox.parser.*
import compilersandbox.tokenizer.Tokenizer.Operator

import scala.collection.mutable
object Compute {

  case class ComputeError()

  def compute(tree: Node): Either[ComputeError, Either[Int, Double]] = {

    val nodes = mutable.Stack(linearize(tree).reverse*)
    val operandStack: mutable.Stack[Operand] = mutable.Stack()

    while (nodes.nonEmpty) {
      nodes.pop() match {
        case OpenParenthesis => ???
        case CloseParenthesis => ???
        case Add =>
          (operandStack.pop(), operandStack.pop()) match {
            case (DecimalOperand(left), DecimalOperand(right)) =>
              operandStack.push(DecimalOperand(left + right))
            case (DecimalOperand(left), IntegerOperand(right)) =>
              operandStack.push(DecimalOperand(left + right))
            case (IntegerOperand(left), DecimalOperand(right)) =>
              operandStack.push(DecimalOperand(left + right))
            case (IntegerOperand(left), IntegerOperand(right)) =>
              operandStack.push(IntegerOperand(left + right))
          }
        case Sub =>
          (operandStack.pop(), operandStack.pop()) match {
            case (DecimalOperand(left), DecimalOperand(right)) =>
              operandStack.push(DecimalOperand(left - right))
            case (DecimalOperand(left), IntegerOperand(right)) =>
              operandStack.push(DecimalOperand(left - right))
            case (IntegerOperand(left), DecimalOperand(right)) =>
              operandStack.push(DecimalOperand(left - right))
            case (IntegerOperand(left), IntegerOperand(right)) =>
              operandStack.push(IntegerOperand(left - right))
          }
        case Mul =>
          (operandStack.pop(), operandStack.pop()) match {
            case (DecimalOperand(left), DecimalOperand(right)) =>
              operandStack.push(DecimalOperand(left * right))
            case (DecimalOperand(left), IntegerOperand(right)) =>
              operandStack.push(DecimalOperand(left * right))
            case (IntegerOperand(left), DecimalOperand(right)) =>
              operandStack.push(DecimalOperand(left * right))
            case (IntegerOperand(left), IntegerOperand(right)) =>
              operandStack.push(IntegerOperand(left * right))
          }
        case Div =>
          val left = toDouble(operandStack.pop())
          val right = toDouble(operandStack.pop())
          val result = left / right
          operandStack.push(DecimalOperand(result))
        case Pow =>
          (operandStack.pop(), operandStack.pop()) match {
            case (DecimalOperand(left), DecimalOperand(right)) =>
              operandStack.push(DecimalOperand(Math.pow(left, right)))
            case (DecimalOperand(left), IntegerOperand(right)) =>
              operandStack.push(DecimalOperand(Math.pow(left, right)))
            case (IntegerOperand(left), DecimalOperand(right)) =>
              operandStack.push(DecimalOperand(Math.pow(left, right)))
            case (IntegerOperand(left), IntegerOperand(right)) =>
              operandStack.push(IntegerOperand(Math.pow(left, right).asInstanceOf[Int]))
          }
        case Sin => ???
        case Cos => ???
        case Tan => ???
        case Fac => ???
        case Sqrt => ???
        case operand: Operand =>
          operandStack.push(operand)
      }
    }

    operandStack.pop() match {
      case DecimalOperand(value) =>
        Right(Right(value))
      case IntegerOperand(value) =>
        Right(Left(value))
    }
  }

  def linearize(tree: Node): List[Operator | Operand] = {

    val result = mutable.ListBuffer[Operator | Operand]()
    val stack = mutable.Stack[Node]()
    stack.push(tree)
    while (stack.nonEmpty) {
      stack.pop() match {
        case OperatorNode(operator, left, right) =>
          result.append(operator)
          stack.push(right, left)
        case OperandNode(operand) =>
          result.append(operand)
      }
    }
    result.toList
  }
  def toDouble(operand: Operand): Double = {
    operand match {
      case DecimalOperand(value) => value
      case IntegerOperand(value) => value.toDouble
    }
  }
}
