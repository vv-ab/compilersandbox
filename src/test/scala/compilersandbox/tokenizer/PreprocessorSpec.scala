package compilersandbox.tokenizer

import Tokens.ParenthesisKind.{Close, Open}
import Tokens.{IntegerLiteral, End, DecimalLiteral, Ident, Literal, Parenthesis, Start, Token}
import org.junit.runner.RunWith
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PreprocessorSpec extends AnyFreeSpec {

  "A Preprocessor" - {

    "should process 2(3+4)" in {

      val input = List(Start, Literal("2"), Parenthesis(Open), Literal("3"), Ident("+"), Literal("4"), Parenthesis(Close), End)
      val expected = List(Start, Literal("2"), Ident("*"), Parenthesis(Open), Literal("3"), Ident("+"), Literal("4"), Parenthesis(Close), End)
      val result = Preprocessor.preprocess(input)
      assert(result == expected)
    }

    "should process (3+4)2" in {

      val input = List(Start, Parenthesis(Open), Literal("3"), Ident("+"), Literal("4"), Parenthesis(Close), Literal("2"), End)
      val expected = List(Start, Parenthesis(Open), Literal("3"), Ident("+"), Literal("4"), Parenthesis(Close), Ident("*"), Literal("2"), End)
      val result = Preprocessor.preprocess(input)
      assert(result == expected)
    }

    "should process 5+3" in {

      val input = List(Start, Literal("5"), Ident("+"), Literal("3"), End)
      val expected = input
      val result = Preprocessor.preprocess(input)
      assert(result == expected)
    }

    "should process 4cos(0)" in {

      val input = List(Start, Literal("4"), Ident("cos"), Parenthesis(Open), Literal("0"), Parenthesis(Close))
      val expected = List(Start, Literal("4"), Ident("*"), Ident("cos"), Parenthesis(Open), Literal("0"), Parenthesis(Close))
      val result = Preprocessor.preprocess(input)
      assert(result == expected)
    }
    
    "should process 4.3cos(0)" in {

      val input = List(Start, DecimalLiteral("4.3"), Ident("cos"), Parenthesis(Open), Literal("0"), Parenthesis(Close))
      val expected = List(Start, DecimalLiteral("4.3"), Ident("*"), Ident("cos"), Parenthesis(Open), Literal("0"), Parenthesis(Close))
      val result = Preprocessor.preprocess(input)
      assert(result == expected)
    }

    "should process 7.5(2-1)" in {

      val input = List(Start, DecimalLiteral("7.5"), Parenthesis(Open), Literal("2"), Ident("-"), Literal("1"), Parenthesis(Close), End)
      val expected = List(Start, DecimalLiteral("7.5"), Ident("*"), Parenthesis(Open), Literal("2"), Ident("-"), Literal("1"), Parenthesis(Close), End)
      val result = Preprocessor.preprocess(input)
      assert(result == expected)
    }

    "should process (2-1)2.5" in {

      val input = List(Start,  Parenthesis(Open), Literal("2"), Ident("-"), Literal("1"), Parenthesis(Close), DecimalLiteral("2.5"), End)
      val expected = List(Start, Parenthesis(Open), Literal("2"), Ident("-"), Literal("1"), Parenthesis(Close), Ident("*"), DecimalLiteral("2.5"), End)
      val result = Preprocessor.preprocess(input)
      assert(result == expected)
    }

    "should process (2-2.5)2" in {

      val input = List(Start, Parenthesis(Open), Literal("2"), Ident("-"), DecimalLiteral("2.5"), Parenthesis(Close), Literal("2"), End)
      val expected = List(Start, Parenthesis(Open), Literal("2"), Ident("-"), DecimalLiteral("2.5"), Parenthesis(Close), Ident("*"), Literal("2"), End)
      val result = Preprocessor.preprocess(input)
      assert(result == expected)
    }

    "should process pi" in {

      val input = List(Start, Ident("pi"), End)
      val expected = List(Start, IntegerLiteral("pi"), End)
      val result = Preprocessor.preprocess(input)
      assert(result == expected)
    }

    "should process 3pi" in {

      val input = List(Start, Literal("3"), Ident("pi"), End)
      val expected = List(Start, Literal("3"), Ident("*"), IntegerLiteral("pi"), End)
      val result = Preprocessor.preprocess(input)
      assert(result == expected)
    }

    "should process pipi" in {

      val input = List(Start, Ident("pi"), Ident("pi"), End)
      val expected = List(Start, IntegerLiteral("pi"), Ident("*"), IntegerLiteral("pi"), End)
      val result = Preprocessor.preprocess(input)
      assert(result == expected)
    }

    "should process e" in {

      val input = List(Start, Ident("e"), End)
      val expected = List(Start, IntegerLiteral("e"), End)
      val result = Preprocessor.preprocess(input)
      assert(result == expected)
    }

    "should process epi" in {

      val input = List(Start, Ident("e"), Ident("pi"), End)
      val expected = List(Start, IntegerLiteral("e"), Ident("*"), IntegerLiteral("pi"), End)
      val result = Preprocessor.preprocess(input)
      assert(result == expected)
    }

    "should process sqrt(9)sqrt(9)" in {

      val input = List(Start, Ident("sqrt"), Parenthesis(Open), Literal("9"), Parenthesis(Close), Ident("sqrt"), Parenthesis(Open), Literal("9"), Parenthesis(Close), End)
      val expected = List(Start, Ident("sqrt"), Parenthesis(Open), Literal("9"), Parenthesis(Close), Ident("*"), Ident("sqrt"), Parenthesis(Open), Literal("9"), Parenthesis(Close), End)
      val result = Preprocessor.preprocess(input)
      assert(result == expected)
    }


    "should process pisqrt(9)" in {

      val input = List(Start, Ident("pi"), Ident("sqrt"), Parenthesis(Open), Literal("9"), Parenthesis(Close), End)
      val expected = List(Start, IntegerLiteral("pi"), Ident("*"), Ident("sqrt"), Parenthesis(Open), Literal("9"), Parenthesis(Close), End)
      val result = Preprocessor.preprocess(input)
      assert(result == expected)
    }
  }
}
