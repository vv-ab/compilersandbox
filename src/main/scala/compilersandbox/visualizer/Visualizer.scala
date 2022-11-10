package compilersandbox.visualizer

import compilersandbox.parser.{Add, CloseParenthesis, Cos, Div, Mul, Node, OpenParenthesis, OperandNode, Operator, OperatorNode, Pow, Sin, Sub, Tan}

object Visualizer {

  def visualize(nodeQueue: List[Node], previous: Int, result: String): String = {
    def operatorToString(operator: Operator): String = {
      operator match {
        case OpenParenthesis => ???
        case CloseParenthesis => ???
        case Add =>
          "+"
        case Sub =>
          "-"
        case Mul =>
          "*"
        case Div =>
          "/"
        case Pow =>
          "^"
        case Sin =>
          "sin"
        case Cos =>
          "cos"
        case Tan =>
          "tan"
      }
    }
    nodeQueue.headOption match {
      case Some(OperatorNode(operator, left, right)) =>
        val operatorString = operatorToString(operator)
        val part =
        s"""|/-------\\
           ||   $operatorString   |
           |\\-------/
           |""".stripMargin

        visualize(nodeQueue.tail :+ left :+ right, previous + 1, result + part)
      case Some(OperandNode(operand)) =>
        val part =
        s"""|/-------\\
            ||   ${operand.value}   |
            |\\-------/
            |""".stripMargin
        visualize(nodeQueue.tail, previous, result + part)
      case None =>
        
        result

    }
  }
}
