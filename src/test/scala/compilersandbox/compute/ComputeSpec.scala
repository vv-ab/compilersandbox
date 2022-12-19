package compilersandbox.compute

import compilersandbox.compute.Compute.ComputeError
import compilersandbox.parser.{Add, Cos, DecimalOperand, Div, Fac, IntegerOperand, Mul, OperandNode, OperatorNode, Pow, Sin, Sqrt, Sub, Tan}
import org.junit.runner.RunWith
import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ComputeSpec extends AnyFreeSpec {

  "Compute" - {

    "should compute" in {

      val tree = OperatorNode(Div, OperatorNode(Add, OperandNode(DecimalOperand(8)), OperandNode(DecimalOperand(2))), OperandNode(DecimalOperand(2)))
      val result = Compute.compute(tree)
      val expected = Right(Right(5.0))

      assert(result == expected)
    }

    "Addition" - {
      "should compute an integer result from 2 IntegerOperands" in {

        val tree = OperatorNode(Add, OperandNode(IntegerOperand(8)), OperandNode(IntegerOperand(2)))
        val result = Compute.compute(tree)
        val expected = Right(Left(10))

        assert(result == expected)
      }

      "should compute an decimal result from 1 IntegerOperand and 1 DecimalOperand" in {

        val tree = OperatorNode(Add, OperandNode(IntegerOperand(8)), OperandNode(DecimalOperand(2.2)))
        val result = Compute.compute(tree)
        val expected = Right(Right(10.2))

        assert(result == expected)
      }

      "should compute an decimal result from 1 DecimalOperand and 1 IntegerOperand" in {

        val tree = OperatorNode(Add, OperandNode(DecimalOperand(8.3)), OperandNode(IntegerOperand(2)))
        val result = Compute.compute(tree)
        val expected = Right(Right(10.3))

        assert(result == expected)
      }

      "should compute an decimal result from 2 DecimalOperand" in {

        val tree = OperatorNode(Add, OperandNode(DecimalOperand(8.3)), OperandNode(DecimalOperand(2.5)))
        val result = Compute.compute(tree)
        val expected = Right(Right(10.8))

        assert(result == expected)
      }
    }

    "Subtraction" - {
      "should compute an integer result from 2 IntegerOperands" in {

        val tree = OperatorNode(Sub, OperandNode(IntegerOperand(8)), OperandNode(IntegerOperand(2)))
        val result = Compute.compute(tree)
        val expected = Right(Left(6))

        assert(result == expected)
      }

      "should compute an decimal result from 1 IntegerOperand and 1 DecimalOperand" in {

        val tree = OperatorNode(Sub, OperandNode(IntegerOperand(8)), OperandNode(DecimalOperand(2.2)))
        val result = Compute.compute(tree)
        val expected = Right(Right(5.8))

        assert(result == expected)
      }

      "should compute an decimal result from 1 DecimalOperand and 1 IntegerOperand" in {

        val tree = OperatorNode(Sub, OperandNode(DecimalOperand(8.5)), OperandNode(IntegerOperand(2)))
        val result = Compute.compute(tree)
        val expected = Right(Right(6.5))

        assert(result == expected)
      }

      "should compute an decimal result from 2 DecimalOperand" in {

        val tree = OperatorNode(Sub, OperandNode(DecimalOperand(8.0)), OperandNode(DecimalOperand(2.5)))
        val result = Compute.compute(tree)
        val expected = Right(Right(5.5))

        assert(result == expected)
      }
    }

    "Multiplication" - {
      "should compute an integer result from 2 IntegerOperands" in {

        val tree = OperatorNode(Mul, OperandNode(IntegerOperand(8)), OperandNode(IntegerOperand(2)))
        val result = Compute.compute(tree)
        val expected = Right(Left(16))

        assert(result == expected)
      }

      "should compute an decimal result from 1 IntegerOperand and 1 DecimalOperand" in {

        val tree = OperatorNode(Mul, OperandNode(IntegerOperand(8)), OperandNode(DecimalOperand(2.5)))
        val result = Compute.compute(tree)
        val expected = Right(Right(20))

        assert(result == expected)
      }

      "should compute an decimal result from 1 DecimalOperand and 1 IntegerOperand" in {

        val tree = OperatorNode(Mul, OperandNode(DecimalOperand(8.5)), OperandNode(IntegerOperand(2)))
        val result = Compute.compute(tree)
        val expected = Right(Right(17))

        assert(result == expected)
      }

      "should compute an decimal result from 2 DecimalOperand" in {

        val tree = OperatorNode(Mul, OperandNode(DecimalOperand(8.0)), OperandNode(DecimalOperand(2.5)))
        val result = Compute.compute(tree)
        val expected = Right(Right(20))

        assert(result == expected)
      }
    }

    "Power" - {
      "should compute an integer result from 2 IntegerOperands" in {

        val tree = OperatorNode(Pow, OperandNode(IntegerOperand(8)), OperandNode(IntegerOperand(2)))
        val result = Compute.compute(tree)
        val expected = Right(Left(64))

        assert(result == expected)
      }

      "should compute an decimal result from 1 IntegerOperand and 1 DecimalOperand" in {

        val tree = OperatorNode(Pow, OperandNode(IntegerOperand(8)), OperandNode(DecimalOperand(2.5)))
        val result = Compute.compute(tree)
        val expected = Right(Right(Math.pow(8, 2.5)))

        assert(result == expected)
      }

      "should compute an decimal result from 1 DecimalOperand and 1 IntegerOperand" in {

        val tree = OperatorNode(Pow, OperandNode(DecimalOperand(2.5)), OperandNode(IntegerOperand(2)))
        val result = Compute.compute(tree)
        val expected = Right(Right(6.25))

        assert(result == expected)
      }

      "should compute an decimal result from 2 DecimalOperand" in {

        val tree = OperatorNode(Pow, OperandNode(DecimalOperand(8.0)), OperandNode(DecimalOperand(2.5)))
        val result = Compute.compute(tree)
        val expected = Right(Right(Math.pow(8.0, 2.5)))

        assert(result == expected)
      }
    }

    "should compute a decimal result from 2(88/22)-2^3" in {

      val tree = OperatorNode(
        Sub, OperatorNode(
            Mul, OperandNode(IntegerOperand(2)), OperatorNode(
              Div, OperandNode(IntegerOperand(88)), OperandNode(IntegerOperand(22)))),
            OperatorNode(
              Pow, OperandNode(IntegerOperand(2)), OperandNode(IntegerOperand(3))))
      val result = Compute.compute(tree)
      val expected = Right(Right(0.0))

      assert(result == expected)
    }

    "Trigonometric expressions" - {

      "should compute sine expression" in {

        val tree = OperatorNode(Sin, OperandNode(DecimalOperand(80.0)), OperandNode(DecimalOperand(0)))
        val result = Compute.compute(tree)
        val expected = Right(Right(Math.sin(Math.toRadians(80.0))))

        assert(result == expected)
      }

      "should compute 7*sin(90)-8" in {

        val tree = OperatorNode(
          Sub, OperatorNode(
            Mul, OperandNode(IntegerOperand(7)), OperatorNode(
              Sin, OperandNode(DecimalOperand(90)), OperandNode(DecimalOperand(0)))),
          OperandNode(IntegerOperand(8)))
        val result = Compute.compute(tree)
        val expected = Right(Right(-1.00))

        assert(result == expected)
      }

      "should compute a cosine expression" in {

        val tree = OperatorNode(Cos, OperandNode(DecimalOperand(80.0)), OperandNode(DecimalOperand(0)))
        val result = Compute.compute(tree)
        val expected = Right(Right(Math.cos(Math.toRadians(80.0))))

        assert(result == expected)
      }

      "should compute a tan expression" in {

        val tree = OperatorNode(Tan, OperandNode(DecimalOperand(80.0)), OperandNode(DecimalOperand(0)))
        val result = Compute.compute(tree)
        val expected = Right(Right(Math.tan(Math.toRadians(80.0))))

        assert(result == expected)
      }

      "should compute tan(88)+sin(120)-3*cos(0)" in {

        val tree =
          OperatorNode(
            Sub, OperatorNode(
            Add, OperatorNode(
              Tan, OperandNode(DecimalOperand(88.0)), OperandNode(DecimalOperand(0.0))),
                 OperatorNode(
              Sin, OperandNode(DecimalOperand(120.0)), OperandNode(DecimalOperand(0.0)))),
            OperatorNode(
              Mul, OperandNode(DecimalOperand(3.0)),
                OperatorNode(
                  Cos, OperandNode(DecimalOperand(0.0)), OperandNode(DecimalOperand(0.0)))))
        val result = Compute.compute(tree)
        val expected = Right(Right(26.502278686699952))

        assert(result == expected)
      }
    }

    "should compute faculty" in {

      val tree = OperatorNode(Fac, OperandNode(IntegerOperand(5)), OperandNode(IntegerOperand(0)))
      val result = Compute.compute(tree)
      val expected = Right(Left(120))

      assert(result == expected)
    }

    "should fail on 3.2!" in {

      val tree = OperatorNode(Fac, OperandNode(DecimalOperand(3.2)), OperandNode(IntegerOperand(0)))
      val result = Compute.compute(tree)
      val expected = Left(ComputeError("cannot compute expression"))

      assert(result == expected)
    }

    "should compute sqrt" in {

      val tree = OperatorNode(Sqrt, OperandNode(IntegerOperand(9)), OperandNode(IntegerOperand(0)))
      val result = Compute.compute(tree)
      val expected = Right(Right(Math.sqrt(9)))

      assert(result == expected)
    }

    "should fail on sqrt(-4)" ignore {

      val tree = OperatorNode(Sqrt, OperandNode(IntegerOperand(-4)), OperandNode(IntegerOperand(0)))
      val result = Compute.compute(tree)
      val expected = Left(ComputeError("cannot compute expression"))

      assert(result == expected)
    }
  }

}
