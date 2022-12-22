package compilersandbox.compute

import compilersandbox.parser.*
import compilersandbox.tokenizer.Tokenizer.Operator
import compilersandbox.util.Failure

import scala.collection.mutable
import scala.util.{Success, Try}
object Compute {

  case class ComputeFailure(message: String) extends Failure

  def compute(tree: Node): Either[List[ComputeFailure], Either[Long, Double]] = {

    val nodes = mutable.Stack(linearize(tree).reverse*)
    val operandStack: mutable.Stack[Operand] = mutable.Stack()

    while (nodes.nonEmpty) {
      nodes.pop() match {
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
        case Sin =>
          val left = toDouble(operandStack.pop())
          val right = toDouble(operandStack.pop())
          operandStack.push(DecimalOperand(Math.sin(Math.toRadians(left))))
        case Cos =>
          val left = toDouble(operandStack.pop())
          val right = toDouble(operandStack.pop())
          operandStack.push(DecimalOperand(Math.cos(Math.toRadians(left))))
        case Tan =>
          val left = toDouble(operandStack.pop())
          operandStack.pop()
          operandStack.push(DecimalOperand(Math.tan(Math.toRadians(left))))
        case Fac =>
          operandStack.pop() match {
            case DecimalOperand(left) =>
              operandStack.pop()
              return Left(List(ComputeFailure("cannot compute expression")))
            case IntegerOperand(value) =>
              if (value > MAX_RECURSION_DEPTH) {
                return Left(List(ComputeFailure(s"cannot compute faculty of numbers greater than $MAX_RECURSION_DEPTH")))
              }
              else {
                operandStack.pop()
                Try(faculty(value)) match {
                  case util.Failure(_) =>
                    return Left(List(ComputeFailure("cannot compute expression due to arithmetic overflow")))
                  case Success(value) =>
                    operandStack.push(IntegerOperand(value))
                }
              }
          }
        case Sqrt =>
          val left = toDouble(operandStack.pop())
          operandStack.pop()
          operandStack.push(DecimalOperand(Math.sqrt(left)))
        case Flo =>
          operandStack.pop() match {
            case DecimalOperand(value) =>
              operandStack.pop()
              operandStack.push(IntegerOperand(value.floor.toLong))
            case IntegerOperand(value) =>
              operandStack.pop()
              operandStack.push(IntegerOperand(value))
          }
        case Ceil =>
          operandStack.pop() match {
            case DecimalOperand(value) =>
              operandStack.pop()
              operandStack.push(IntegerOperand(value.ceil.toLong))
            case IntegerOperand(value) =>
              operandStack.pop()
              operandStack.push(IntegerOperand(value))
          }
        case Round =>
          operandStack.pop() match {
            case DecimalOperand(value) =>
              operandStack.pop()
              operandStack.push(IntegerOperand(value.round))
            case IntegerOperand(value) =>
              operandStack.pop()
              operandStack.push(IntegerOperand(value))
          }
        case operand: Operand =>
          operandStack.push(operand)
        case OpenParenthesis => ???
        case CloseParenthesis => ???
      }
    }
      operandStack.pop() match {
        case DecimalOperand(value) =>
          Right(Right(value))
        case IntegerOperand(value) =>
          Right(Left(value))
      }
  }


  val MAX_RECURSION_DEPTH = 6046
  def faculty(x: Long): Long = {
    if (x == 0) {
      1
    }
    else {
      Math.multiplyExact(x, faculty(x - 1))
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
