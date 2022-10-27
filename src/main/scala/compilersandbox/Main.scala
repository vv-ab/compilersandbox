package compilersandbox

import compilersandbox.model.*

import scala.collection.mutable

@main
def main(): Unit = {
  print("Enter expression: ")
  val input = Console.in.readLine()
  val tree = parse(input, mutable.Stack.empty, mutable.Stack.empty)
  val result = tree.compute()
  println(s"Result: $result")
}

def parse(input: Seq[Char], operatorStack: mutable.Stack[Operator], nodeStack: mutable.Stack[Node]): Node = {

  def insertOperator(operator: Operator, operatorStack: mutable.Stack[Operator], nodeStack: mutable.Stack[Node]): Unit = {
    operatorStack.headOption match {
      case Some(head) if head.precedence() >= operator.precedence() =>
        val right = nodeStack.pop()
        val left = nodeStack.pop()
        val operatorNode = OperatorNode(operatorStack.pop(), left, right)
        nodeStack.push(operatorNode)
        insertOperator(operator, operatorStack, nodeStack)
      case _ =>
        operatorStack.push(operator)
    }
  }

  def drainOperatorStack(operatorStack: mutable.Stack[Operator], nodeStack: mutable.Stack[Node]): Node = {
    if (operatorStack.nonEmpty) {
      val right = nodeStack.pop()
      val left = nodeStack.pop()
      val operator = operatorStack.pop()
      val operatorNode = OperatorNode(operator, left, right)
      nodeStack.push(operatorNode)
      drainOperatorStack(operatorStack, nodeStack)
    }
    else {
      nodeStack.pop()
    }
  }

  input.headOption match {
    case Some(character) =>
      character match {
        case '+' =>
          insertOperator(Add, operatorStack, nodeStack)
        case '-' =>
          insertOperator(Sub, operatorStack, nodeStack)
        case '*' =>
          insertOperator(Mul, operatorStack, nodeStack)
        case '/' =>
          insertOperator(Div, operatorStack, nodeStack)
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          nodeStack.push(OperandNode(Operand(character.asDigit)))
        case _ =>
      }
      parse(input.tail, operatorStack, nodeStack)
    case None =>
      val result = drainOperatorStack(operatorStack, nodeStack)
      result
  }
}
