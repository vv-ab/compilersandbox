package compilersandbox.compute

import compilersandbox.parser.{Add, DecimalOperand, Div, IntegerOperand, Mul, OperandNode, OperatorNode, Pow, Sub}
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


  }

}
