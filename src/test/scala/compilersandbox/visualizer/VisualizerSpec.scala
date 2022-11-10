package compilersandbox.visualizer

import compilersandbox.parser.{Add, Operand, OperandNode, OperatorNode}
import org.junit.runner.RunWith
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class VisualizerSpec extends AnyFreeSpec {

  "A Visualizer" - {

    "should visualize 1" in {

      val tree = OperandNode(Operand(1))
      val expected =
        """|/-------\
           ||   1   |
           |\-------/
           |""".stripMargin
      val result = Visualizer.visualize(List(tree),0, "")

      assert(result == expected)
    }

    "should visualize 1+2 (easy)" in {

      val tree = OperatorNode(Add, OperandNode(Operand(1)), OperandNode(Operand(2)))
      val expected =
        """|/-------\
           ||   +   |
           |\-------/
           |/-------\
           ||   1   |
           |\-------/
           |/-------\
           ||   2   |
           |\-------/
           |""".stripMargin
      val result = Visualizer.visualize(List(tree), 0, "")

      assert(result == expected)
      println(result)
    }

    "should visualize 1+2 (medium)" in {

      val tree = OperatorNode(Add, OperandNode(Operand(1)), OperandNode(Operand(2)))
      val expected =
        """|/-------\
           ||   +   |
           |\-------/
           |/-------\ /-------\
           ||   1   | |   2   |
           |\-------/ \-------/
           |""".stripMargin
      val result = Visualizer.visualize(List(tree), 0, "")

      assert(result == expected)
      println(result)
    }

    "should visualize 1+2" in {

      val tree = OperatorNode(Add, OperandNode(Operand(1)), OperandNode(Operand(2)))
      val expected =
        """|          /-------\
           |          |   +   |
           |          \-------/
           |              ^
           |              |
           |    +---------+---------+
           |    |                   |
           |/-------\           /-------\
           ||   1   |           |   2   |
           |\-------/           \-------/
           |""".stripMargin
      val result = Visualizer.visualize(List(tree), 0, "")

      assert(result == expected)
    }
  }
}
