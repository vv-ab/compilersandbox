package compilersandbox.tokenizer

import compilersandbox.tokenizer.ParenthesisKind.{Close, Open}
import compilersandbox.tokenizer.{End, Number, Operator, Parenthesis, Preprocessor, Start, Token, Tokenizer}
import org.scalatest.freespec.AnyFreeSpec


class PreprocessorSpec extends AnyFreeSpec {

  "A Preprocessor" - {

    "should process 2(3+4)" in {

      val input = List(Start, Number("2"), Parenthesis(Open), Number("3"), Operator("+"), Number("4"), Parenthesis(Close), End)
      val expected = List(Start, Number("2"), Operator("*"), Parenthesis(Open), Number("3"), Operator("+"), Number("4"), Parenthesis(Close), End)
      val result = Preprocessor.preprocess(input, List.empty)
      assert(result == expected)
    }

    "should process (3+4)2" in {

      val input = List(Start, Parenthesis(Open), Number("3"), Operator("+"), Number("4"), Parenthesis(Close), Number("2"), End)
      val expected = List(Start, Parenthesis(Open), Number("3"), Operator("+"), Number("4"), Parenthesis(Close), Operator("*"), Number("2"), End)
      val result = Preprocessor.preprocess(input, List.empty)
      assert(result == expected)
    }

    "should process 5+3" in {

      val input = List(Start, Number("5"), Operator("+"), Number("3"), End)
      val expected = input
      val result = Preprocessor.preprocess(input, List.empty)
      assert(result == expected)
    }

    "should process 4cos(0)" in {

    }
  }
}
