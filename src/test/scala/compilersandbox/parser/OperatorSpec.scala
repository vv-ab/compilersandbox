package compilersandbox.parser

import org.junit.runner.RunWith
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class OperatorSpec extends AnyFreeSpec {

  "An Add Operator" - {

    "should compute 2+3" in {
      assert(Add.compute(2, 3) == 2 + 3)
    }
  }

  "A Sub Operator" - {

    "should compute 2-3" in {
      assert(Sub.compute(2, 3) == 4 - 3)
    }
  }

  "A Mul Operator" - {

    "should compute 2*3" in {
      assert(Mul.compute(2, 3) == 2 * 3)
    }
  }

  "A Div Operator" - {

    "should compute 6/3" in {
      assert(Div.compute(6, 3) == 6 / 3)
    }
  }

  "A Pow Operator" - {

    "should compute 2^3" in {
      assert(Pow.compute(2, 3) == Math.pow(2, 3))
    }
  }

  "A Sin Operator" - {

    "should compute sin(90)" in {
      assert(Sin.compute(90, 0) == Math.sin(Math.toRadians(90)))
    }
  }

  "A Cos Operator" - {

    "should compute cos(0)" in {
      assert(Cos.compute(0, 0) == Math.cos(Math.toRadians(0)))
    }
  }

  "A Tan Operator" - {

    "should compute tan(50)" in {
      assert(Tan.compute(50, 0) == Math.tan(Math.toRadians(50)).asInstanceOf[Int])
    }
  }
}
