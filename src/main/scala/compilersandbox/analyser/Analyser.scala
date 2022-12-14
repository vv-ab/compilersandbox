package compilersandbox.analyser

import compilersandbox.parser.DataType.Decimal
import compilersandbox.parser.{Add, Ceil, CloseParenthesis, Cos, DataType, DecimalOperand, Div, Fac, Flo, IntegerOperand, Mul, Node, OpenParenthesis, OperandNode, OperatorNode, Pow, Round, Sin, Sqrt, Sub, Tan}
import compilersandbox.util.Failure

import scala.annotation.tailrec
import scala.collection.mutable

object Analyser {

  case class AnalyseFailure(node: Node, message: String) extends Failure

  def analyse(tree: Node): Either[List[AnalyseFailure], Node] = {

    val leafsToParents = Analyser.parents(tree)

    val result = leafsToParents
      .map({ case (node: OperandNode, parents) =>
        checkDataType(node, parents)
      })
      .partition(_.isLeft) match {
        case (Nil, _) => Right(tree)
        case (errors, _) => Left(for(Left(s) <- errors) yield s)
      }

    result
  }

  def checkDataType(node: OperandNode, parents: List[Node]): Either[AnalyseFailure, Unit] = {

    @tailrec def check(currentNode: Node, currentDataType: DataType, remaining: List[Node]): Either[AnalyseFailure, Unit] = {

      val result = currentNode match {
        case OperatorNode(operator, left, right) =>
          operator match {
            case Add | Sub | Mul | Pow if currentDataType == DataType.Decimal =>
              Right(DataType.Decimal)
            case Add | Sub | Mul | Pow =>
              Right(DataType.Integer)
            case Div =>
              Right(DataType.Decimal)
            case Sin | Cos | Tan | Sqrt =>
              Right(DataType.Decimal)
            case Fac if currentDataType == DataType.Decimal =>
              Left(AnalyseFailure(currentNode, "Expected integer number for Faculty but got decimal!"))
            case Fac =>
              Right(DataType.Integer)
            case Flo =>
              Right(DataType.Integer)
            case Ceil =>
              Right(DataType.Integer)
            case Round =>
              Right(DataType.Integer)
            case OpenParenthesis => ???
            case CloseParenthesis => ???
          }
        case OperandNode(operand) =>
          Right(operand.dataType)
        }

      result match {
        case Left(value) =>

          Left(value)
        case Right(_) if remaining.isEmpty =>
          Right(())
        case Right(dataType) =>
          check(remaining.head, dataType, remaining.tail)
      }
    }

    check(node, node.operand.dataType, parents)
  }

  def parents(tree: Node): List[(OperandNode, List[Node])] = {

    val stack = mutable.Stack[(Node, List[Node])]()
    val result = mutable.ListBuffer[(OperandNode, List[Node])]()

    stack.push((tree, List.empty))

    while (stack.nonEmpty) {
      val (node, parents) = stack.pop()
      node match {
        case OperatorNode(_, left, right) =>
          stack.push((left, node +: parents))
          stack.push((right, node +: parents))
        case operand: OperandNode =>
          result.append((operand, node +: parents))
      }
    }

    result.toList
  }
}
