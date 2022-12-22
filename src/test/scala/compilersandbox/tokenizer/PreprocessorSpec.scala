package compilersandbox.tokenizer

import Tokens.ParenthesisKind.{Close, Open}
import Tokens.{ConstantLiteral, End, DecimalLiteral, Ident, IntegerLiteral, Parenthesis, Start, Token}
import org.junit.runner.RunWith
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PreprocessorSpec extends AnyFreeSpec {

  "A Preprocessor" - {

    "should process 2(3+4)" in {

      val input = List(Start, IntegerLiteral("2"), Parenthesis(Open), IntegerLiteral("3"), Ident("+"), IntegerLiteral("4"), Parenthesis(Close), End)
      val expected = List(Start, IntegerLiteral("2"), Ident("*"), Parenthesis(Open), IntegerLiteral("3"), Ident("+"), IntegerLiteral("4"), Parenthesis(Close), End)
      val result = Preprocessor.preprocess(input)
      assert(result == expected)
    }

    "should process (3+4)2" in {

      val input = List(Start, Parenthesis(Open), IntegerLiteral("3"), Ident("+"), IntegerLiteral("4"), Parenthesis(Close), IntegerLiteral("2"), End)
      val expected = List(Start, Parenthesis(Open), IntegerLiteral("3"), Ident("+"), IntegerLiteral("4"), Parenthesis(Close), Ident("*"), IntegerLiteral("2"), End)
      val result = Preprocessor.preprocess(input)
      assert(result == expected)
    }

    "should process 5+3" in {

      val input = List(Start, IntegerLiteral("5"), Ident("+"), IntegerLiteral("3"), End)
      val expected = input
      val result = Preprocessor.preprocess(input)
      assert(result == expected)
    }

    "should process 4cos(0)" in {

      val input = List(Start, IntegerLiteral("4"), Ident("cos"), Parenthesis(Open), IntegerLiteral("0"), Parenthesis(Close))
      val expected = List(Start, IntegerLiteral("4"), Ident("*"), Ident("cos"), Parenthesis(Open), IntegerLiteral("0"), Parenthesis(Close))
      val result = Preprocessor.preprocess(input)
      assert(result == expected)
    }
    
    "should process 4.3cos(0)" in {

      val input = List(Start, DecimalLiteral("4.3"), Ident("cos"), Parenthesis(Open), IntegerLiteral("0"), Parenthesis(Close))
      val expected = List(Start, DecimalLiteral("4.3"), Ident("*"), Ident("cos"), Parenthesis(Open), IntegerLiteral("0"), Parenthesis(Close))
      val result = Preprocessor.preprocess(input)
      assert(result == expected)
    }

    "should process 7.5(2-1)" in {

      val input = List(Start, DecimalLiteral("7.5"), Parenthesis(Open), IntegerLiteral("2"), Ident("-"), IntegerLiteral("1"), Parenthesis(Close), End)
      val expected = List(Start, DecimalLiteral("7.5"), Ident("*"), Parenthesis(Open), IntegerLiteral("2"), Ident("-"), IntegerLiteral("1"), Parenthesis(Close), End)
      val result = Preprocessor.preprocess(input)
      assert(result == expected)
    }

    "should process (2-1)2.5" in {

      val input = List(Start,  Parenthesis(Open), IntegerLiteral("2"), Ident("-"), IntegerLiteral("1"), Parenthesis(Close), DecimalLiteral("2.5"), End)
      val expected = List(Start, Parenthesis(Open), IntegerLiteral("2"), Ident("-"), IntegerLiteral("1"), Parenthesis(Close), Ident("*"), DecimalLiteral("2.5"), End)
      val result = Preprocessor.preprocess(input)
      assert(result == expected)
    }

    "should process (2-2.5)2" in {

      val input = List(Start, Parenthesis(Open), IntegerLiteral("2"), Ident("-"), DecimalLiteral("2.5"), Parenthesis(Close), IntegerLiteral("2"), End)
      val expected = List(Start, Parenthesis(Open), IntegerLiteral("2"), Ident("-"), DecimalLiteral("2.5"), Parenthesis(Close), Ident("*"), IntegerLiteral("2"), End)
      val result = Preprocessor.preprocess(input)
      assert(result == expected)
    }

    "should process pi" in {

      val input = List(Start, Ident("pi"), End)
      val expected = List(Start, ConstantLiteral("pi"), End)
      val result = Preprocessor.preprocess(input)
      assert(result == expected)
    }

    "should process 3pi" in {

      val input = List(Start, IntegerLiteral("3"), Ident("pi"), End)
      val expected = List(Start, IntegerLiteral("3"), Ident("*"), ConstantLiteral("pi"), End)
      val result = Preprocessor.preprocess(input)
      assert(result == expected)
    }

    "should process pipi" in {

      val input = List(Start, Ident("pi"), Ident("pi"), End)
      val expected = List(Start, ConstantLiteral("pi"), Ident("*"), ConstantLiteral("pi"), End)
      val result = Preprocessor.preprocess(input)
      assert(result == expected)
    }

    "should process e" in {

      val input = List(Start, Ident("e"), End)
      val expected = List(Start, ConstantLiteral("e"), End)
      val result = Preprocessor.preprocess(input)
      assert(result == expected)
    }

    "should process epi" in {

      val input = List(Start, Ident("e"), Ident("pi"), End)
      val expected = List(Start, ConstantLiteral("e"), Ident("*"), ConstantLiteral("pi"), End)
      val result = Preprocessor.preprocess(input)
      assert(result == expected)
    }

    "should process sqrt(9)sqrt(9)" in {

      val input = List(Start, Ident("sqrt"), Parenthesis(Open), IntegerLiteral("9"), Parenthesis(Close), Ident("sqrt"), Parenthesis(Open), IntegerLiteral("9"), Parenthesis(Close), End)
      val expected = List(Start, Ident("sqrt"), Parenthesis(Open), IntegerLiteral("9"), Parenthesis(Close), Ident("*"), Ident("sqrt"), Parenthesis(Open), IntegerLiteral("9"), Parenthesis(Close), End)
      val result = Preprocessor.preprocess(input)
      assert(result == expected)
    }

    "should process pisqrt(9)" in {

      val input = List(Start, Ident("pi"), Ident("sqrt"), Parenthesis(Open), IntegerLiteral("9"), Parenthesis(Close), End)
      val expected = List(Start, ConstantLiteral("pi"), Ident("*"), Ident("sqrt"), Parenthesis(Open), IntegerLiteral("9"), Parenthesis(Close), End)
      val result = Preprocessor.preprocess(input)
      assert(result == expected)
    }

    "should process pifloor(9,8)" in {

      val input = List(Start, Ident("pi"), Ident("floor"), Parenthesis(Open), DecimalLiteral("9,8"), Parenthesis(Close), End)
      val expected = List(Start, ConstantLiteral("pi"), Ident("*"), Ident("floor"), Parenthesis(Open), DecimalLiteral("9.8"), Parenthesis(Close), End)
      val result = Preprocessor.preprocess(input)
      assert(result == expected)
    }

    "should process eceil(9,8)" in {

      val input = List(Start, Ident("e"), Ident("ceil"), Parenthesis(Open), DecimalLiteral("9,8"), Parenthesis(Close), End)
      val expected = List(Start, ConstantLiteral("e"), Ident("*"), Ident("ceil"), Parenthesis(Open), DecimalLiteral("9.8"), Parenthesis(Close), End)
      val result = Preprocessor.preprocess(input)
      assert(result == expected)
    }

    "should process 2,3" in {

      val input = List(Start, DecimalLiteral("2,3"), End)
      val expected = List(Start, DecimalLiteral("2.3"), End)
      val result = Preprocessor.preprocess(input)
      assert(result == expected)
    }
  }
}
