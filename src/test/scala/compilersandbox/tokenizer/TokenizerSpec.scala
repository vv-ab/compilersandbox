package compilersandbox.tokenizer

import compilersandbox.tokenizer.ParenthesisKind.{Close, Open}
import compilersandbox.tokenizer.{End, Number, Operator, Parenthesis, Start, Tokenizer}
import org.junit.runner.RunWith
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TokenizerSpec extends AnyFreeSpec {

  "A Tokenizer" - {

    "should tokenize 59+3" in {

      val expected = Right(List(Start, Number("59"), Operator("+"), Number("3"), End))
      val result = Tokenizer.tokenize("59+3", Start, List.empty)
      assert(result == expected)
    }

    "should tokenize (5+3)" in {

      val expected = Right(List(Start, Parenthesis(Open), Number("5"), Operator("+"), Number("3"), Parenthesis(Close), End))
      val result = Tokenizer.tokenize("(5+3)", Start, List.empty)
      assert(result == expected)
    }

    "should tokenize 5*(8-1)" in {

      val expected = Right(List(Start, Number("5"), Operator("*"), Parenthesis(Open), Number("8"), Operator("-"), Number("1"), Parenthesis(Close), End))
      val result = Tokenizer.tokenize("5*(8-1)", Start, List.empty)
      assert(result == expected)

    }

    "should tokenize (-8+1)" in {

      val expected = Right(List(Start, Parenthesis(Open), Number("-8"), Operator("+"), Number("1"), Parenthesis(Close), End))
      val result = Tokenizer.tokenize("(-8+1)", Start, List.empty)
      assert(result == expected)
    }

    "should tokenize -8+1" in {

      val expected = Right(List(Start, Number("-8"), Operator("+"), Number("1"), End))
      val result = Tokenizer.tokenize("-8+1", Start, List.empty)
      assert(result == expected)
    }

    "should tokenize -84+12" in {

      val expected = Right(List(Start, Number("-84"), Operator("+"), Number("12"), End))
      val result = Tokenizer.tokenize("-84+12", Start, List.empty)
      assert(result == expected)
    }

    "should tokenize (100-84)+12" in {

      val expected = Right(List(Start, Parenthesis(Open), Number("100"), Operator("-"), Number("84"), Parenthesis(Close), Operator("+"), Number("12"), End))
      val result = Tokenizer.tokenize("(100-84)+12", Start, List.empty)
      assert(result == expected)
    }

    "should tokenize 2^3" in {

      val expected = Right(List(Start, Number("2"), Operator("^"), Number("3"), End))
      val result = Tokenizer.tokenize("2^3", Start, List.empty)
      assert(result == expected)
    }

    "should tokenize  2*sin(90)-22" in {

      val expected = Right(List(Start, Number("2"), Operator("*"), Operator("sin"), Parenthesis(Open), Number("90"), Parenthesis(Close), Operator("-"), Number("22"), End))
      val result = Tokenizer.tokenize("2*sin(90)-22", Start, List.empty)
      assert(result == expected)
    }

    "should tokenize cos(0)" in {

      val expected = Right(List(Start, Operator("cos"), Parenthesis(Open), Number("0"), Parenthesis(Close), End))
      val result = Tokenizer.tokenize("cos(0)", Start, List.empty)
      assert(result == expected)
    }

    "should tokenize tan(50)" in {

      val expected = Right(List(Start, Operator("tan"), Parenthesis(Open), Number("50"), Parenthesis(Close), End))
      val result = Tokenizer.tokenize("tan(50)", Start, List.empty)
      assert(result == expected)
    }

    "should tokenize 4(2/1)" in {

      val expected = Right(List(Start, Number("4"), Parenthesis(Open), Number("2"), Operator("/"), Number("1"), Parenthesis(Close), End))
      val result = Tokenizer.tokenize("4(2/1)", Start, List.empty)
      assert(result == expected)
    }

    "should tokenize (2+1)(7-3)" in {

      val expected = Right(List(Start, Parenthesis(Open), Number("2"), Operator("+"), Number("1"), Parenthesis(Close), Parenthesis(Open), Number("7"), Operator("-"), Number("3"), Parenthesis(Close), End))
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

      val expected = Right(List(Start, Number("6"), Operator("*"), Operator("/"), Number("3"), End))
      val result = Tokenizer.tokenize("6*/3", Start, List.empty)
      assert(result == expected)
    }

    "should fail on ()" ignore {

      val expected = Right(List(Start, Parenthesis(Open), Parenthesis(Close), End))
      val result = Tokenizer.tokenize("()", Start, List.empty)
      assert(result == expected)
    }

    "should fail on 6+*1" ignore {

      val expected = Right(List(Start, Number("6"), Operator("+"), Operator("*"), Number("1"), End))
      val result = Tokenizer.tokenize("6+*1", Start, List.empty)
      assert(result == expected)
    }

    "should fail on 2/+" ignore {

      val expected = Right(List(Start, Number("2"), Operator("/"), Operator("+"), End))
      val result = Tokenizer.tokenize("2/+", Start, List.empty)
      assert(result == expected)
    }
  }
}
