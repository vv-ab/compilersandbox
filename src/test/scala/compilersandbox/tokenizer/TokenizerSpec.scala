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

      val input = "r"
      val expected = Left(TokenizerFailure("unexpected token", input, Location(0)))
      val result = Tokenizer.tokenize(input, Start, List.empty)
      assert(result == expected)
    }

    "should fail on u+w" in {

      val input = "u+w"
      val expected = Left(TokenizerFailure("unexpected token", input, Location(0)))
      val result = Tokenizer.tokenize(input, Start, List.empty)
      assert(result == expected)
    }

    "should fail on 5+u" in {

      val input = "5+u"
      val expected = Left(TokenizerFailure("unexpected token", input, Location(2)))
      val result = Tokenizer.tokenize(input, Start, List.empty)
      assert(result == expected)
    }

    "should fail on 1+s" in {

      val input = "1+s"
      val expected = Left(TokenizerFailure("incomplete operator", input, Location(3)))
      val result = Tokenizer.tokenize(input, Start, List.empty)
      assert(result == expected)
    }

    "should fail on ta+1" in {

      val input = "ta+1"
      val expected = Left(TokenizerFailure("incomplete operator", input, Location(2)))
      val result = Tokenizer.tokenize(input, Start, List.empty)
      assert(result == expected)
    }

    "should fail on 1+t" in {

      val input = "1+t"
      val expected = Left(TokenizerFailure("incomplete operator", input, Location(3)))
      val result = Tokenizer.tokenize(input, Start, List.empty)
      assert(result == expected)
    }

    "should fail on sin(x)" in {

      val input = "sin(x)"
      val expected = Left(TokenizerFailure("unexpected token", input, Location(4)))
      val result = Tokenizer.tokenize(input, Start, List.empty)
      assert(result == expected)
    }

    "should fail on 6*/3" in {

      val input = "6*/3"
      val expected = Left(TokenizerFailure("unexpected operator", input, Location(2)))
      val result = Tokenizer.tokenize(input, Start, List.empty)
      assert(result == expected)
    }

    "should fail on ()" in {

      val input = "()"
      val expected = Left(TokenizerFailure("missing operand", input, Location(1)))
      val result = Tokenizer.tokenize(input, Start, List.empty)
      assert(result == expected)
    }

    "should fail on 6+*1" in {

      val input = "6+*1"
      val expected = Left(TokenizerFailure("unexpected operator", input, Location(2)))
      val result = Tokenizer.tokenize(input, Start, List.empty)
      assert(result == expected)
    }

    "should fail on 2/+" in {

      val input = "2/+"
      val expected = Left(TokenizerFailure("unexpected operator", input, Location(2)))
      val result = Tokenizer.tokenize(input, Start, List.empty)
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

      val input = "3.4."
      val expected = Left(TokenizerFailure("unexpected token", input, Location(3)))
      val result = Tokenizer.tokenize(input, Start, List.empty)
      assert(result == expected)
    }

    "should fail on 6.-8" in {

      val input = "6.-8"
      val expected = Left(TokenizerFailure("unexpected token", input, Location(2)))
      val result = Tokenizer.tokenize(input, Start, List.empty)
      assert(result == expected)
    }

    "should fail on 6./8" in {

      val input = "6./8"
      val expected = Left(TokenizerFailure("unexpected token", input, Location(2)))
      val result = Tokenizer.tokenize(input, Start, List.empty)
      assert(result == expected)
    }

    "should fail on 6.cos(0)" in {

      val input = "6.cos(0)"
      val expected = Left(TokenizerFailure("unexpected token", input, Location(2)))
      val result = Tokenizer.tokenize(input, Start, List.empty)
      assert(result == expected)
    }

    "should fail on 6.(2+2)" in {

      val input = "6.(2+2)"
      val expected = Left(TokenizerFailure("unexpected token", input, Location(2)))
      val result = Tokenizer.tokenize(input, Start, List.empty)
      assert(result == expected)
    }

    "should fail on (2+2.)" in {

      val input = "(2+2.)"
      val expected = Left(TokenizerFailure("unexpected token", input, Location(5)))
      val result = Tokenizer.tokenize(input, Start, List.empty)
      assert(result == expected)
    }

    "should fail on 2..2" in {

      val input = "2..2"
      val expected = Left(TokenizerFailure("unexpected token", input, Location(2)))
      val result = Tokenizer.tokenize(input, Start, List.empty)
      assert(result == expected)
    }

    "should fail on .3" in {

      val input = ".3"
      val expected = Left(TokenizerFailure("unexpected token", input, Location(0)))
      val result = Tokenizer.tokenize(input, Start, List.empty)
      assert(result == expected)
    }

    "should tokenize (1+2)cos(3)" in {

      val expected = Right(List(Start, Parenthesis(Open), IntegerNumber("1"), Operator("+"), IntegerNumber("2"), Parenthesis(Close), Operator("cos"), Parenthesis(Open), IntegerNumber("3"), Parenthesis(Close), End))
      val result = Tokenizer.tokenize("(1+2)cos(3)", Start, List.empty)
      assert(result == expected)
    }

    "should fail on 5++4" in {

      val input = "5++4"
      val expected = Left(TokenizerFailure("unexpected operator", input, Location(2)))
      val result = Tokenizer.tokenize(input, Start, List.empty)
      assert(result == expected)
    }
  }
}
