package compilersandbox.tokenizer

import compilersandbox.tokenizer.Tokenizer.ParenthesisKind.{Close, Open}
import compilersandbox.tokenizer.Tokenizer.{End, FloatingPointLiteral, Ident, Literal, Parenthesis, Start, Token}
import org.junit.runner.RunWith
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PreprocessorSpec extends AnyFreeSpec {

  "A Preprocessor" - {

    "should process 2(3+4)" in {

      val input = List(Start, Literal("2"), Parenthesis(Open), Literal("3"), Ident("+"), Literal("4"), Parenthesis(Close), End)
      val expected = List(Start, Literal("2"), Ident("*"), Parenthesis(Open), Literal("3"), Ident("+"), Literal("4"), Parenthesis(Close), End)
      val result = Preprocessor.preprocess(input, List.empty)
      assert(result == expected)
    }

    "should process (3+4)2" in {

      val input = List(Start, Parenthesis(Open), Literal("3"), Ident("+"), Literal("4"), Parenthesis(Close), Literal("2"), End)
      val expected = List(Start, Parenthesis(Open), Literal("3"), Ident("+"), Literal("4"), Parenthesis(Close), Ident("*"), Literal("2"), End)
      val result = Preprocessor.preprocess(input, List.empty)
      assert(result == expected)
    }

    "should process 5+3" in {

      val input = List(Start, Literal("5"), Ident("+"), Literal("3"), End)
      val expected = input
      val result = Preprocessor.preprocess(input, List.empty)
      assert(result == expected)
    }

    "should process 4cos(0)" in {

      val input = List(Start, Literal("4"), Ident("cos"), Parenthesis(Open), Literal("0"), Parenthesis(Close))
      val expected = List(Start, Literal("4"), Ident("*"), Ident("cos"), Parenthesis(Open), Literal("0"), Parenthesis(Close))
      val result = Preprocessor.preprocess(input, List.empty)
      assert(result == expected)
    }
    
    "should process 4.3cos(0)" in {

      val input = List(Start, FloatingPointLiteral("4.3"), Ident("cos"), Parenthesis(Open), Literal("0"), Parenthesis(Close))
      val expected = List(Start, FloatingPointLiteral("4.3"), Ident("*"), Ident("cos"), Parenthesis(Open), Literal("0"), Parenthesis(Close))
      val result = Preprocessor.preprocess(input, List.empty)
      assert(result == expected)
    }

    "should process 7.5(2-1)" in {

      val input = List(Start, FloatingPointLiteral("7.5"), Parenthesis(Open), Literal("2"), Ident("-"), Literal("1"), Parenthesis(Close), End)
      val expected = List(Start, FloatingPointLiteral("7.5"), Ident("*"), Parenthesis(Open), Literal("2"), Ident("-"), Literal("1"), Parenthesis(Close), End)
      val result = Preprocessor.preprocess(input, List.empty)
      assert(result == expected)
    }

    "should process (2-1)2.5" in {

      val input = List(Start,  Parenthesis(Open), Literal("2"), Ident("-"), Literal("1"), Parenthesis(Close), FloatingPointLiteral("2.5"), End)
      val expected = List(Start, Parenthesis(Open), Literal("2"), Ident("-"), Literal("1"), Parenthesis(Close), Ident("*"), FloatingPointLiteral("2.5"), End)
      val result = Preprocessor.preprocess(input, List.empty)
      assert(result == expected)
    }

    "should process (2-2.5)2" in {

      val input = List(Start, Parenthesis(Open), Literal("2"), Ident("-"), FloatingPointLiteral("2.5"), Parenthesis(Close), Literal("2"), End)
      val expected = List(Start, Parenthesis(Open), Literal("2"), Ident("-"), FloatingPointLiteral("2.5"), Parenthesis(Close), Ident("*"), Literal("2"), End)
      val result = Preprocessor.preprocess(input, List.empty)
      assert(result == expected)
    }
  }
}
