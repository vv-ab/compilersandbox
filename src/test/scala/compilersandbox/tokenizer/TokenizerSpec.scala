package compilersandbox.tokenizer

import compilersandbox.tokenizer.ParenthesisKind.{Close, Open}
import compilersandbox.tokenizer.{End, Number, Operator, Parenthesis, Start, Tokenizer}
import org.scalatest.freespec.AnyFreeSpec

class TokenizerSpec extends AnyFreeSpec {

  "A Tokenizer" - {

    "should tokenize 59+3" in {

      val expected = List(Start, Number("59"), Operator("+"), Number("3"), End)
      val result = Tokenizer.tokenize("59+3", Start, List.empty)
      assert(result == expected)
    }

    "should tokenize (5+3)" in {

      val expected = List(Start, Parenthesis(Open), Number("5"), Operator("+"), Number("3"), Parenthesis(Close), End)
      val result = Tokenizer.tokenize("(5+3)", Start, List.empty)
      assert(result == expected)
    }

    "should tokenize 5*(8-1)" in {

      val expected = List(Start, Number("5"), Operator("*"), Parenthesis(Open), Number("8"), Operator("-"), Number("1"), Parenthesis(Close), End)
      val result = Tokenizer.tokenize("5*(8-1)", Start, List.empty)
      assert(result == expected)

    }

    "should tokenize (-8+1)" in {

      val expected = List(Start, Parenthesis(Open), Number("-8"), Operator("+"), Number("1"), Parenthesis(Close), End)
      val result = Tokenizer.tokenize("(-8+1)", Start, List.empty)
      assert(result == expected)
    }

    "should tokenize -8+1" in {

      val expected = List(Start, Number("-8"), Operator("+"), Number("1"), End)
      val result = Tokenizer.tokenize("-8+1", Start, List.empty)
      assert(result == expected)
    }

    "should tokenize -84+12" in {

      val expected = List(Start, Number("-84"), Operator("+"), Number("12"), End)
      val result = Tokenizer.tokenize("-84+12", Start, List.empty)
      assert(result == expected)
    }

    "should tokenize (100-84)+12" in {

      val expected = List(Start, Parenthesis(Open), Number("100"), Operator("-"), Number("84"), Parenthesis(Close), Operator("+"), Number("12"), End)
      val result = Tokenizer.tokenize("(100-84)+12", Start, List.empty)
      assert(result == expected)
    }

    "should tokenize 2^3" in {

      val expected = List(Start, Number("2"), Operator("^"), Number("3"), End)
      val result = Tokenizer.tokenize("2^3", Start, List.empty)
      assert(result == expected)
    }
  }
}
