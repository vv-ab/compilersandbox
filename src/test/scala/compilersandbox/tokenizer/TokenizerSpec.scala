package compilersandbox.tokenizer

import compilersandbox.parser.OperatorNode
import compilersandbox.tokenizer.ParenthesisKind.{Close, Open}
import compilersandbox.tokenizer.{End, IntegerNumber, Operator, Parenthesis, Start, Tokenizer}
import org.junit.runner.RunWith
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TokenizerSpec extends AnyFreeSpec {

  "A Tokenizer" - {

    "should tokenize 59+3" in {

      val expected = Right(List(Start, IntegerNumber("59"), Operator("+"), IntegerNumber("3"), End))
      val result = Tokenizer.tokenize("59+3", Start, List.empty)
      assert(result == expected)
    }

    "should tokenize (5+3)" in {

      val expected = Right(List(Start, Parenthesis(Open), IntegerNumber("5"), Operator("+"), IntegerNumber("3"), Parenthesis(Close), End))
      val result = Tokenizer.tokenize("(5+3)", Start, List.empty)
      assert(result == expected)
    }

    "should tokenize 5*(8-1)" in {

      val expected = Right(List(Start, IntegerNumber("5"), Operator("*"), Parenthesis(Open), IntegerNumber("8"), Operator("-"), IntegerNumber("1"), Parenthesis(Close), End))
      val result = Tokenizer.tokenize("5*(8-1)", Start, List.empty)
      assert(result == expected)

    }

    "should tokenize (-8+1)" in {

      val expected = Right(List(Start, Parenthesis(Open), IntegerNumber("-8"), Operator("+"), IntegerNumber("1"), Parenthesis(Close), End))
      val result = Tokenizer.tokenize("(-8+1)", Start, List.empty)
      assert(result == expected)
    }

    "should tokenize -8+1" in {

      val expected = Right(List(Start, IntegerNumber("-8"), Operator("+"), IntegerNumber("1"), End))
      val result = Tokenizer.tokenize("-8+1", Start, List.empty)
      assert(result == expected)
    }

    "should tokenize -84+12" in {

      val expected = Right(List(Start, IntegerNumber("-84"), Operator("+"), IntegerNumber("12"), End))
      val result = Tokenizer.tokenize("-84+12", Start, List.empty)
      assert(result == expected)
    }

    "should tokenize (100-84)+12" in {

      val expected = Right(List(Start, Parenthesis(Open), IntegerNumber("100"), Operator("-"), IntegerNumber("84"), Parenthesis(Close), Operator("+"), IntegerNumber("12"), End))
      val result = Tokenizer.tokenize("(100-84)+12", Start, List.empty)
      assert(result == expected)
    }

    "should tokenize 2^3" in {

      val expected = Right(List(Start, IntegerNumber("2"), Operator("^"), IntegerNumber("3"), End))
      val result = Tokenizer.tokenize("2^3", Start, List.empty)
      assert(result == expected)
    }

    "should tokenize  2*sin(90)-22" in {

      val expected = Right(List(Start, IntegerNumber("2"), Operator("*"), Operator("sin"), Parenthesis(Open), IntegerNumber("90"), Parenthesis(Close), Operator("-"), IntegerNumber("22"), End))
      val result = Tokenizer.tokenize("2*sin(90)-22", Start, List.empty)
      assert(result == expected)
    }

    "should tokenize cos(0)" in {

      val expected = Right(List(Start, Operator("cos"), Parenthesis(Open), IntegerNumber("0"), Parenthesis(Close), End))
      val result = Tokenizer.tokenize("cos(0)", Start, List.empty)
      assert(result == expected)
    }

    "should tokenize tan(50)" in {

      val expected = Right(List(Start, Operator("tan"), Parenthesis(Open), IntegerNumber("50"), Parenthesis(Close), End))
      val result = Tokenizer.tokenize("tan(50)", Start, List.empty)
      assert(result == expected)
    }

    "should tokenize 4(2/1)" in {

      val expected = Right(List(Start, IntegerNumber("4"), Parenthesis(Open), IntegerNumber("2"), Operator("/"), IntegerNumber("1"), Parenthesis(Close), End))
      val result = Tokenizer.tokenize("4(2/1)", Start, List.empty)
      assert(result == expected)
    }

    "should tokenize (2+1)(7-3)" in {

      val expected = Right(List(Start, Parenthesis(Open), IntegerNumber("2"), Operator("+"), IntegerNumber("1"), Parenthesis(Close), Parenthesis(Open), IntegerNumber("7"), Operator("-"), IntegerNumber("3"), Parenthesis(Close), End))
      val result = Tokenizer.tokenize("(2+1)(7-3)", Start, List.empty)
      assert(result == expected)
    }

    "should fail on r" in {

      val expected = Left(TokenizerFailure("unexpected token"))
      val result = Tokenizer.tokenize("r", Start, List.empty)
      assert(result == expected)
    }

    "should fail on u+w" in {

      val expected = Left(TokenizerFailure("unexpected token"))
      val result = Tokenizer.tokenize("u+w", Start, List.empty)
      assert(result == expected)
    }

    "should fail on 5+u" in {

      val expected = Left(TokenizerFailure("unexpected token"))
      val result = Tokenizer.tokenize("5+u", Start, List.empty)
      assert(result == expected)
    }

    "should fail on 1+s" in {

      val expected = Left(TokenizerFailure("incomplete operator"))
      val result = Tokenizer.tokenize("1+s", Start, List.empty)
      assert(result == expected)
    }

    "should fail on ta+1" in {

      val expected = Left(TokenizerFailure("incomplete operator"))
      val result = Tokenizer.tokenize("ta+1", Start, List.empty)
      assert(result == expected)
    }

    "should fail on 1+t" in {

      val expected = Left(TokenizerFailure("incomplete operator"))
      val result = Tokenizer.tokenize("1+t", Start, List.empty)
      assert(result == expected)
    }

    "should fail on sin(x)" in {

      val expected = Left(TokenizerFailure("unexpected token"))
      val result = Tokenizer.tokenize("sin(x)", Start, List.empty)
      assert(result == expected)
    }

    "should fail on 6*/3" ignore {

      val expected = Right(List(Start, IntegerNumber("6"), Operator("*"), Operator("/"), IntegerNumber("3"), End))
      val result = Tokenizer.tokenize("6*/3", Start, List.empty)
      assert(result == expected)
    }

    "should fail on ()" ignore {

      val expected = Right(List(Start, Parenthesis(Open), Parenthesis(Close), End))
      val result = Tokenizer.tokenize("()", Start, List.empty)
      assert(result == expected)
    }

    "should fail on 6+*1" ignore {

      val expected = Right(List(Start, IntegerNumber("6"), Operator("+"), Operator("*"), IntegerNumber("1"), End))
      val result = Tokenizer.tokenize("6+*1", Start, List.empty)
      assert(result == expected)
    }

    "should fail on 2/+" ignore {

      val expected = Right(List(Start, IntegerNumber("2"), Operator("/"), Operator("+"), End))
      val result = Tokenizer.tokenize("2/+", Start, List.empty)
      assert(result == expected)
    }

    "should tokenize 2.25-4.3" in {

      val expected = Right(List(Start, FloatingPointNumber("2.25"), Operator("-"), FloatingPointNumber("4.3"), End))
      val result = Tokenizer.tokenize("2.25-4.3", Start, List.empty)
      assert(result == expected)

    }

    "should tokenize 25-4.3" in {

      val expected = Right(List(Start, IntegerNumber("25"), Operator("-"), FloatingPointNumber("4.3"), End))
      val result = Tokenizer.tokenize("25-4.3", Start, List.empty)
      assert(result == expected)

    }

    "should tokenize 3.2*0.2" in {

      val expected = Right(List(Start, FloatingPointNumber("3.2"), Operator("*"), FloatingPointNumber("0.2"), End))
      val result = Tokenizer.tokenize("3.2*0.2", Start, List.empty)
      assert(result == expected)
    }

    "should tokenize 2.8(1.5^2)" in {

      val expected = Right(List(Start, FloatingPointNumber("2.8"), Parenthesis(Open), FloatingPointNumber("1.5"), Operator("^"), IntegerNumber("2"), Parenthesis(Close), End))
      val result = Tokenizer.tokenize("2.8(1.5^2)", Start, List.empty)
      assert(result == expected)
    }

    "should tokenize 5(7*1.1)3" in {

      val expected = Right(List(Start, IntegerNumber("5"), Parenthesis(Open), IntegerNumber("7"), Operator("*"), FloatingPointNumber("1.1"), Parenthesis(Close), IntegerNumber("3"), End))
      val result = Tokenizer.tokenize("5(7*1.1)3", Start, List.empty)
      assert(result == expected)
    }

    "should tokenize sin(22.77)" in {

      val expected = Right(List(Start, Operator("sin"), Parenthesis(Open), FloatingPointNumber("22.77"), Parenthesis(Close), End))
      val result = Tokenizer.tokenize("sin(22.77)", Start, List.empty)
      assert(result == expected)
    }

    "should fail on 3.4." in {

      val expected = Left(TokenizerFailure("unexpected token"))
      val result = Tokenizer.tokenize("3.4.", Start, List.empty)
      assert(result == expected)
    }

    "should fail on 6.-8" in {

      val expected = Left(TokenizerFailure("unexpected token"))
      val result = Tokenizer.tokenize("6.-8", Start, List.empty)
      assert(result == expected)
    }

    "should fail on 6./8" in {

      val expected = Left(TokenizerFailure("unexpected token"))
      val result = Tokenizer.tokenize("6./8", Start, List.empty)
      assert(result == expected)
    }

    "should fail on 6.cos(0)" in {

      val expected = Left(TokenizerFailure("unexpected token"))
      val result = Tokenizer.tokenize("6./8", Start, List.empty)
      assert(result == expected)
    }

    "should fail on 6.(2+2)" in {

      val expected = Left(TokenizerFailure("unexpected token"))
      val result = Tokenizer.tokenize("6.(2+2)", Start, List.empty)
      assert(result == expected)
    }

    "should fail on (2+2.)" in {

      val expected = Left(TokenizerFailure("unexpected token"))
      val result = Tokenizer.tokenize("(2+2.)", Start, List.empty)
      assert(result == expected)
    }

    "should fail on 2..2" in {

      val expected = Left(TokenizerFailure("unexpected token"))
      val result = Tokenizer.tokenize("2..2", Start, List.empty)
      assert(result == expected)
    }

    "should fail on .3" in {

    val expected = Left(TokenizerFailure("unexpected token"))
    val result = Tokenizer.tokenize(".3", Start, List.empty)
    assert(result == expected)
    }

    "should tokenize (1+2)cos(3)" in {

      val expected = Right(List(Start, Parenthesis(Open), IntegerNumber("1"), Operator("+"), IntegerNumber("2"), Parenthesis(Close), Operator("cos"), Parenthesis(Open), IntegerNumber("3"), Parenthesis(Close), End))
      val result = Tokenizer.tokenize("(1+2)cos(3)", Start, List.empty)
      assert(result == expected)
    }

  }
}
