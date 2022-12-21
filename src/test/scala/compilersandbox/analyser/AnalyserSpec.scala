package compilersandbox.analyser

import compilersandbox.analyser.Analyser.AnalyseFailure
import compilersandbox.compute.Compute
import compilersandbox.parser.*
import org.junit.runner.RunWith
import org.scalatest.Inside
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class AnalyserSpec extends AnyFreeSpec with Inside {

  "An Analyser" - {

    "should analyse 5.5!" in {

      val input = OperatorNode(Fac, OperandNode(DecimalOperand(5.5)), OperandNode(IntegerOperand(0)))
      val expected = List(AnalyseFailure(input, "Expected integer number for Faculty but got decimal!"))

      val result = Analyser.analyse(input)
      inside(result) { case Left(result) =>
        assert(result == expected)
      }
    }

    "should analyse (10/3)!" in {

      val input = OperatorNode(Fac, OperatorNode(Div,OperandNode(IntegerOperand(10)), OperandNode(IntegerOperand(3))),OperandNode(IntegerOperand(0)))
      val expected = List(AnalyseFailure(input, "Expected integer number for Faculty but got decimal!"),
        AnalyseFailure(input, "Expected integer number for Faculty but got decimal!"))

      val result = Analyser.analyse(input)
      inside(result) { case Left(result) =>
        assert(result == expected)
      }
    }

    "should analyse 1.5!*0.2!" in {

      val input = OperatorNode(Mul, OperatorNode(Fac,OperandNode(DecimalOperand(1.5)),OperandNode(IntegerOperand(0))),
        OperatorNode(Fac,OperandNode(DecimalOperand(0.2)),OperandNode(IntegerOperand(0))))
      val expected = List(AnalyseFailure(OperatorNode(Fac,OperandNode(DecimalOperand(0.2)),OperandNode(IntegerOperand(0))),"Expected integer number for Faculty but got decimal!"),
        AnalyseFailure(OperatorNode(Fac,OperandNode(DecimalOperand(1.5)),OperandNode(IntegerOperand(0))),"Expected integer number for Faculty but got decimal!"))

      val result = Analyser.analyse(input)
      inside(result) { case Left(result) =>
        assert(result == expected)
      }
    }

    "should analyse sin(30)" in {

      val input = OperatorNode(Sin, OperandNode(IntegerOperand(30)), OperandNode(IntegerOperand(0)))
      val expected = OperatorNode(Sin,OperandNode(IntegerOperand(30)),OperandNode(IntegerOperand(0)))

      val result = Analyser.analyse(input)
      inside(result) { case Right(result) =>
        assert(result == expected)
      }
    }

    "should analyse 10/3" in {

      val input = OperatorNode(Div, OperandNode(IntegerOperand(10)), OperandNode(IntegerOperand(3)))
      val expected = OperatorNode(Div, OperandNode(IntegerOperand(10)), OperandNode(IntegerOperand(3)))

      val result = Analyser.analyse(input)
      inside(result) { case Right(result) =>
        assert(result == expected)
      }
    }
  }
}
