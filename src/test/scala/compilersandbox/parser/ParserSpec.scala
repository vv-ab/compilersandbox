package compilersandbox.parser

import compilersandbox.tokenizer.ParenthesisKind.{Close, Open}
import compilersandbox.tokenizer.{End, FloatingPointNumber, IntegerNumber, Operator, Parenthesis, ParenthesisKind, Start}
import org.junit.runner.RunWith
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.junit.JUnitRunner

import scala.collection.mutable

@RunWith(classOf[JUnitRunner])
class ParserSpec extends AnyFreeSpec {

  "A Parser" - {

    "should parse 5+3" in {

      val input = List(Start, IntegerNumber("5"), Operator("+"), IntegerNumber("3"), End)
      val expectation = Right(OperatorNode(Add, OperandNode(Operand(5)), OperandNode(Operand(3))))

      val result = Parser.parse(input, mutable.Stack.empty, mutable.Stack.empty)

      assert(result == expectation)
    }

    "should parse an expression with subtraction" in {

      val input = List(Start, IntegerNumber("7"), Operator("-"), IntegerNumber("2"), End)
      val expectation = Right(OperatorNode(Sub, OperandNode(Operand(7)), OperandNode(Operand(2))))

      val result = Parser.parse(input, mutable.Stack.empty, mutable.Stack.empty)

      assert(result == expectation)
    }

    "should parse 8*2" in {

      val input = List(Start, IntegerNumber("8"), Operator("*"), IntegerNumber("2"), End)
      val expectation = Right(OperatorNode(Mul, OperandNode(Operand(8)), OperandNode(Operand(2))))

      val result = Parser.parse(input, mutable.Stack.empty, mutable.Stack.empty)

      assert(result == expectation)
    }

    "should parse 2*(1+1)" in {

      val input = List(Start, IntegerNumber("2"), Operator("*"), Parenthesis(Open), IntegerNumber("1"), Operator("+"), IntegerNumber("1"), Parenthesis(Close), End)
      val expectation = Right(OperatorNode(Mul, OperandNode(Operand(2)), OperatorNode(Add, OperandNode(Operand(1)), OperandNode(Operand(1)))))

      val result = Parser.parse(input, mutable.Stack.empty, mutable.Stack.empty)

      assert(result == expectation)
    }

    "should parse 2*(1*(3+4))" in {

      val input = List(Start, IntegerNumber("2"), Operator("*"), Parenthesis(Open), IntegerNumber("1"), Operator("*"), Parenthesis(Open), IntegerNumber("3"), Operator("+"), IntegerNumber("4"), Parenthesis(Close), Parenthesis(Close), End)
      val expectation = Right(OperatorNode(Mul, OperandNode(Operand(2)), OperatorNode(Mul, OperandNode(Operand(1)), OperatorNode(Add, OperandNode(Operand(3)), OperandNode(Operand(4))))))

      val result = Parser.parse(input, mutable.Stack.empty, mutable.Stack.empty)

      assert(result == expectation)
    }

    "should parse an expression with division" in {

      val input = List(Start, IntegerNumber("4"), Operator("/"), IntegerNumber("2"), End)
      val expectation = Right(OperatorNode(Div, OperandNode(Operand(4)), OperandNode(Operand(2))))

      val result = Parser.parse(input, mutable.Stack.empty, mutable.Stack.empty)

      assert(result == expectation)
    }

    "should parse expressions with parentheses" in {

      val input = List(Start, Parenthesis(Open), IntegerNumber("8"), Operator("-"), IntegerNumber("6"), Parenthesis(Close), End)
      val expectation = Right(OperatorNode(Sub, OperandNode(Operand(8)), OperandNode(Operand(6))))

      val result = Parser.parse(input, mutable.Stack.empty, mutable.Stack.empty)

      assert(result == expectation)
    }

    "should parse expressions with power" in {

      val input = List(Start, IntegerNumber("3"), Operator("^"), IntegerNumber("3"), End)
      val expectation = Right(OperatorNode(Pow, OperandNode(Operand(3)), OperandNode(Operand(3))))

      val result = Parser.parse(input, mutable.Stack.empty, mutable.Stack.empty)

      assert(result == expectation)
    }

    "should parse 2*(2+2^2)" in {

      val input = List(Start, IntegerNumber("2"), Operator("*"), Parenthesis(Open), IntegerNumber("2"), Operator("+"), IntegerNumber("2"), Operator("^"), IntegerNumber("2"), Parenthesis(Close), End)
      val expectation = Right(OperatorNode(Mul, OperandNode(Operand(2)), OperatorNode(Add, OperandNode(Operand(2)), OperatorNode(Pow, OperandNode(Operand(2)), OperandNode(Operand(2))))))

      val result = Parser.parse(input, mutable.Stack.empty, mutable.Stack.empty)

      assert(result == expectation)
    }

    "should parse 2*(2+(2^2))" in {

      val input = List(Start, IntegerNumber("2"), Operator("*"), Parenthesis(Open), IntegerNumber("2"), Operator("+"), Parenthesis(Open), IntegerNumber("2"), Operator("^"), IntegerNumber("2"), Parenthesis(Close), Parenthesis(Close), End)
      val expectation = Right(OperatorNode(Mul, OperandNode(Operand(2)), OperatorNode(Add, OperandNode(Operand(2)), OperatorNode(Pow, OperandNode(Operand(2)), OperandNode(Operand(2))))))

      val result = Parser.parse(input, mutable.Stack.empty, mutable.Stack.empty)

      assert(result == expectation)
    }

    "should parse trigonometric expressions (sine)" in {

      val input=  List(Start, Operator("sin"), Parenthesis(Open), IntegerNumber("90"), Parenthesis(Close), End)
      val expectation = Right(OperatorNode(Sin, OperandNode(Operand(90)), OperandNode(Operand(0))))

      val result = Parser.parse(input, mutable.Stack.empty, mutable.Stack.empty)

      assert(result == expectation)
    }

    "should parse 2*(sin(90))-22" in {

      val input = List(Start, IntegerNumber("2"), Operator("*"), Parenthesis(Open), Operator("sin"), Parenthesis(Open), IntegerNumber("90"), Parenthesis(Close), Parenthesis(Close), Operator("-"), IntegerNumber("22"), End)
      val expectation = Right(OperatorNode(Sub, OperatorNode(Mul, OperandNode(Operand(2)), OperatorNode(Sin, OperandNode(Operand(90)), OperandNode(Operand(0)))), OperandNode(Operand(22))))

      val result = Parser.parse(input, mutable.Stack.empty, mutable.Stack.empty)

      assert(result == expectation)
    }

    "should parse trigonometric expressions (cosine)" in {

      val input = List(Start, Operator("cos"), Parenthesis(Open), IntegerNumber("0"), Parenthesis(Close), End)
      val expectation = Right(OperatorNode(Cos, OperandNode(Operand(0)), OperandNode(Operand(0))))

      val result = Parser.parse(input, mutable.Stack.empty, mutable.Stack.empty)

      assert(result == expectation)
    }

    "should parse 2*(cos(0))-22" in {

      val input = List(Start, IntegerNumber("2"), Operator("*"), Parenthesis(Open), Operator("cos"), Parenthesis(Open), IntegerNumber("0"), Parenthesis(Close), Parenthesis(Close), Operator("-"), IntegerNumber("22"), End)
      val expectation = Right(OperatorNode(Sub, OperatorNode(Mul, OperandNode(Operand(2)), OperatorNode(Cos, OperandNode(Operand(0)), OperandNode(Operand(0)))), OperandNode(Operand(22))))

      val result = Parser.parse(input, mutable.Stack.empty, mutable.Stack.empty)

      assert(result == expectation)
    }

    "should parse trigonometric expressions (tangents)" in {

      val input = List(Start, Operator("tan"), Parenthesis(Open), IntegerNumber("50"), Parenthesis(Close), End)
      val expectation = Right(OperatorNode(Tan, OperandNode(Operand(50)), OperandNode(Operand(0))))

      val result = Parser.parse(input, mutable.Stack.empty, mutable.Stack.empty)

      assert(result == expectation)
    }

    "should parse 2*(tan(50))-22" in {

      val input = List(Start, IntegerNumber("2"), Operator("*"), Parenthesis(Open), Operator("tan"), Parenthesis(Open), IntegerNumber("50"), Parenthesis(Close), Parenthesis(Close), Operator("-"), IntegerNumber("22"), End)
      val expectation = Right(OperatorNode(Sub, OperatorNode(Mul, OperandNode(Operand(2)), OperatorNode(Tan, OperandNode(Operand(50)), OperandNode(Operand(0)))), OperandNode(Operand(22))))

      val result = Parser.parse(input, mutable.Stack.empty, mutable.Stack.empty)

      assert(result == expectation)
    }

    "should fail on 6*/3" in {

      val input = List(Start, IntegerNumber("6"), Operator("*"), Operator("/"), IntegerNumber("3"), End)
      val expectation = Left(ParsingFailure("missing operand"))

      val result = Parser.parse(input, mutable.Stack.empty, mutable.Stack.empty)

      assert(result == expectation)
    }

    "should fail on 6-/3" in {

      val input = List(Start, IntegerNumber("6"), Operator("-"), Operator("/"), IntegerNumber("3"), End)
      val expectation = Left(ParsingFailure("missing operand"))

      val result = Parser.parse(input, mutable.Stack.empty, mutable.Stack.empty)

      assert(result == expectation)
    }

    "should parse 8" in {

      val input = List(Start, IntegerNumber("8"), End)
      val expectation = Right(OperandNode(Operand(8)))

      val result = Parser.parse(input, mutable.Stack.empty, mutable.Stack.empty)

      assert(result == expectation)
    }

    "should fail on *8" in {

      val input = List(Start, Operator("*"), IntegerNumber("8"), End)
      val expectation = Left(ParsingFailure("missing operand"))

      val result = Parser.parse(input, mutable.Stack.empty, mutable.Stack.empty)

      assert(result == expectation)
    }

    "should fail on *2+2" in {

      val input = List(Start, Operator("*"), IntegerNumber("8"), Operator("+"), IntegerNumber("2"), End)
      val expectation = Left(ParsingFailure("missing operand"))

      val result = Parser.parse(input, mutable.Stack.empty, mutable.Stack.empty)

      assert(result == expectation)
    }

    "should fail on 3+5*" in {

      val input = List(Start, IntegerNumber("3"), Operator("+"), IntegerNumber("5"), Operator("*"), End)
      val expectation = Left(ParsingFailure("missing operand"))

      val result = Parser.parse(input, mutable.Stack.empty, mutable.Stack.empty)

      assert(result == expectation)
    }

    "should fail on sin" in {

      val input = List(Start, Operator("sin"), End)
      val expectation = Left(ParsingFailure("missing operand"))

      val result = Parser.parse(input, mutable.Stack.empty, mutable.Stack.empty)

      assert(result == expectation)
    }

    "should fail on /" in {

      val input = List(Start, Operator("/"), End)
      val expectation = Left(ParsingFailure("missing operand"))

      val result = Parser.parse(input, mutable.Stack.empty, mutable.Stack.empty)

      assert(result == expectation)
    }

    "should fail on ()" in {

      val input = List(Start, Parenthesis(Open), Parenthesis(Close), End)
      val expectation = Left(ParsingFailure(""))

      val result = Parser.parse(input, mutable.Stack.empty, mutable.Stack.empty)

      assert(result == expectation)
    }


    "should parse 6.5-8" in {

      val input = List(Start, FloatingPointNumber("6.5"), Operator("-"), IntegerNumber("8"))
      val expectation = Right(OperatorNode(Sub, OperandNode(Operand(6.5)), OperandNode(Operand(8))))

      val result = Parser.parse(input, mutable.Stack.empty, mutable.Stack.empty)

      assert(result == expectation)
    }
  }
}
