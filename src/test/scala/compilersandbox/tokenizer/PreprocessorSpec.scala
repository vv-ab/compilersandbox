package compilersandbox.tokenizer

import compilersandbox.tokenizer.ParenthesisKind.{Close, Open}
import compilersandbox.tokenizer.{End, IntegerNumber, Operator, Parenthesis, Preprocessor, Start, Token, Tokenizer}
import org.junit.runner.RunWith
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PreprocessorSpec extends AnyFreeSpec {

  "A Preprocessor" - {

    "should process 2(3+4)" in {

      val input = List(Start, IntegerNumber("2"), Parenthesis(Open), IntegerNumber("3"), Operator("+"), IntegerNumber("4"), Parenthesis(Close), End)
      val expected = List(Start, IntegerNumber("2"), Operator("*"), Parenthesis(Open), IntegerNumber("3"), Operator("+"), IntegerNumber("4"), Parenthesis(Close), End)
      val result = Preprocessor.preprocess(input, List.empty)
      assert(result == expected)
    }

    "should process (3+4)2" in {

      val input = List(Start, Parenthesis(Open), IntegerNumber("3"), Operator("+"), IntegerNumber("4"), Parenthesis(Close), IntegerNumber("2"), End)
      val expected = List(Start, Parenthesis(Open), IntegerNumber("3"), Operator("+"), IntegerNumber("4"), Parenthesis(Close), Operator("*"), IntegerNumber("2"), End)
      val result = Preprocessor.preprocess(input, List.empty)
      assert(result == expected)
    }

    "should process 5+3" in {

      val input = List(Start, IntegerNumber("5"), Operator("+"), IntegerNumber("3"), End)
      val expected = input
      val result = Preprocessor.preprocess(input, List.empty)
      assert(result == expected)
    }

    "should process 4cos(0)" in {

      val input = List(Start, IntegerNumber("4"), Operator("cos"), Parenthesis(Open), IntegerNumber("0"), Parenthesis(Close))
      val expected = List(Start, IntegerNumber("4"), Operator("*"), Operator("cos"), Parenthesis(Open), IntegerNumber("0"), Parenthesis(Close))
      val result = Preprocessor.preprocess(input, List.empty)
      assert(result == expected)
    }
    
    "should process 4.3cos(0)" in {

      val input = List(Start, FloatingPointNumber("4.3"), Operator("cos"), Parenthesis(Open), IntegerNumber("0"), Parenthesis(Close))
      val expected = List(Start, FloatingPointNumber("4.3"), Operator("*"), Operator("cos"), Parenthesis(Open), IntegerNumber("0"), Parenthesis(Close))
      val result = Preprocessor.preprocess(input, List.empty)
      assert(result == expected)
    }

    "should process 7.5(2-1)" in {

      val input = List(Start, FloatingPointNumber("7.5"), Parenthesis(Open), IntegerNumber("2"), Operator("-"), IntegerNumber("1"), Parenthesis(Close), End)
      val expected = List(Start, FloatingPointNumber("7.5"), Operator("*"), Parenthesis(Open), IntegerNumber("2"), Operator("-"), IntegerNumber("1"), Parenthesis(Close), End)
      val result = Preprocessor.preprocess(input, List.empty)
      assert(result == expected)
    }

    "should process (2-1)2.5" in {

      val input = List(Start,  Parenthesis(Open), IntegerNumber("2"), Operator("-"), IntegerNumber("1"), Parenthesis(Close), FloatingPointNumber("2.5"), End)
      val expected = List(Start, Parenthesis(Open), IntegerNumber("2"), Operator("-"), IntegerNumber("1"), Parenthesis(Close), Operator("*"), FloatingPointNumber("2.5"), End)
      val result = Preprocessor.preprocess(input, List.empty)
      assert(result == expected)
    }

    "should process (2-2.5)2" in {

      val input = List(Start, Parenthesis(Open), IntegerNumber("2"), Operator("-"), FloatingPointNumber("2.5"), Parenthesis(Close), IntegerNumber("2"), End)
      val expected = List(Start, Parenthesis(Open), IntegerNumber("2"), Operator("-"), FloatingPointNumber("2.5"), Parenthesis(Close), Operator("*"), IntegerNumber("2"), End)
      val result = Preprocessor.preprocess(input, List.empty)
      assert(result == expected)
    }
  }
}
