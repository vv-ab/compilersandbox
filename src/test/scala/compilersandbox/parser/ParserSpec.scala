package compilersandbox.parser

import compilersandbox.tokenizer.Tokens.ParenthesisKind.{Close, Open}
import compilersandbox.tokenizer.Tokens.{ConstantLiteral, End, FloatingPointLiteral, Ident, Literal, Parenthesis, ParenthesisKind, Start, Token}
import compilersandbox.util.Location
import org.junit.runner.RunWith
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.junit.JUnitRunner

import scala.collection.mutable

@RunWith(classOf[JUnitRunner])
class ParserSpec extends AnyFreeSpec {

  "A Parser" - {

    "should parse 5+3" in {

      val input: List[Token] = List(Start, Literal("5"), Ident("+"), Literal("3"), End)
      val expectation = Right(OperatorNode(Add, OperandNode(IntegerOperand(5)), OperandNode(IntegerOperand(3))))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should parse an expression with subtraction" in {

      val input = List(Start, FloatingPointLiteral("7.5"), Ident("-"), Literal("2"), End)
      val expectation = Right(OperatorNode(Sub, OperandNode(DecimalOperand(7.5)), OperandNode(IntegerOperand(2))))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should parse 8*2" in {

      val input = List(Start, Literal("8"), Ident("*"), Literal("2"), End)
      val expectation = Right(OperatorNode(Mul, OperandNode(IntegerOperand(8)), OperandNode(IntegerOperand(2))))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should parse 2*(1+1)" in {

      val input = List(Start, Literal("2"), Ident("*"), Parenthesis(Open), Literal("1"), Ident("+"), Literal("1"), Parenthesis(Close), End)
      val expectation = Right(OperatorNode(Mul, OperandNode(IntegerOperand(2)), OperatorNode(Add, OperandNode(IntegerOperand(1)), OperandNode(IntegerOperand(1)))))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should parse 2*(1*(3+4))" in {

      val input = List(Start, Literal("2"), Ident("*"), Parenthesis(Open), Literal("1"), Ident("*"), Parenthesis(Open), Literal("3"), Ident("+"), Literal("4"), Parenthesis(Close), Parenthesis(Close), End)
      val expectation = Right(OperatorNode(Mul, OperandNode(IntegerOperand(2)), OperatorNode(Mul, OperandNode(IntegerOperand(1)), OperatorNode(Add, OperandNode(IntegerOperand(3)), OperandNode(IntegerOperand(4))))))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should parse an expression with division" in {

      val input = List(Start, Literal("4"), Ident("/"), Literal("2"), End)
      val expectation = Right(OperatorNode(Div, OperandNode(IntegerOperand(4)), OperandNode(IntegerOperand(2))))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should parse expressions with parentheses" in {

      val input = List(Start, Parenthesis(Open), Literal("8"), Ident("-"), Literal("6"), Parenthesis(Close), End)
      val expectation = Right(OperatorNode(Sub, OperandNode(IntegerOperand(8)), OperandNode(IntegerOperand(6))))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should parse expressions with power" in {

      val input = List(Start, Literal("3"), Ident("^"), Literal("3"), End)
      val expectation = Right(OperatorNode(Pow, OperandNode(IntegerOperand(3)), OperandNode(IntegerOperand(3))))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should parse 2*(2+2^2)" in {

      val input = List(Start, Literal("2"), Ident("*"), Parenthesis(Open), Literal("2"), Ident("+"), Literal("2"), Ident("^"), Literal("2"), Parenthesis(Close), End)
      val expectation = Right(OperatorNode(Mul, OperandNode(IntegerOperand(2)), OperatorNode(Add, OperandNode(IntegerOperand(2)), OperatorNode(Pow, OperandNode(IntegerOperand(2)), OperandNode(IntegerOperand(2))))))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should parse 2*(2+(2^2))" in {

      val input = List(Start, Literal("2"), Ident("*"), Parenthesis(Open), Literal("2"), Ident("+"), Parenthesis(Open), Literal("2"), Ident("^"), Literal("2"), Parenthesis(Close), Parenthesis(Close), End)
      val expectation = Right(OperatorNode(Mul, OperandNode(IntegerOperand(2)), OperatorNode(Add, OperandNode(IntegerOperand(2)), OperatorNode(Pow, OperandNode(IntegerOperand(2)), OperandNode(IntegerOperand(2))))))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should parse trigonometric expressions (sine)" in {

      val input = List(Start, Ident("sin"), Parenthesis(Open), Literal("90"), Parenthesis(Close), End)
      val expectation = Right(OperatorNode(Sin, OperandNode(IntegerOperand(90)), OperandNode(DecimalOperand(0.0))))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should parse 2*(sin(90))-22" in {

      val input = List(Start, Literal("2"), Ident("*"), Parenthesis(Open), Ident("sin"), Parenthesis(Open), Literal("90"), Parenthesis(Close), Parenthesis(Close), Ident("-"), Literal("22"), End)
      val expectation = Right(OperatorNode(Sub, OperatorNode(Mul, OperandNode(IntegerOperand(2)), OperatorNode(Sin, OperandNode(IntegerOperand(90)), OperandNode(DecimalOperand(0.0)))), OperandNode(IntegerOperand(22))))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should parse trigonometric expressions (cosine)" in {

      val input = List(Start, Ident("cos"), Parenthesis(Open), Literal("0"), Parenthesis(Close), End)
      val expectation = Right(OperatorNode(Cos, OperandNode(IntegerOperand(0)), OperandNode(DecimalOperand(0.0))))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should parse 2*(cos(0))-22" in {

      val input = List(Start, Literal("2"), Ident("*"), Parenthesis(Open), Ident("cos"), Parenthesis(Open), Literal("0"), Parenthesis(Close), Parenthesis(Close), Ident("-"), Literal("22"), End)
      val expectation = Right(OperatorNode(Sub, OperatorNode(Mul, OperandNode(IntegerOperand(2)), OperatorNode(Cos, OperandNode(IntegerOperand(0)), OperandNode(DecimalOperand(0.0)))), OperandNode(IntegerOperand(22))))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should parse trigonometric expressions (tangents)" in {

      val input = List(Start, Ident("tan"), Parenthesis(Open), Literal("50"), Parenthesis(Close), End)
      val expectation = Right(OperatorNode(Tan, OperandNode(IntegerOperand(50)), OperandNode(DecimalOperand(0.0))))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should parse 2*(tan(50))-22" in {

      val input = List(Start, Literal("2"), Ident("*"), Parenthesis(Open), Ident("tan"), Parenthesis(Open), Literal("50"), Parenthesis(Close), Parenthesis(Close), Ident("-"), Literal("22"), End)
      val expectation = Right(OperatorNode(Sub, OperatorNode(Mul, OperandNode(IntegerOperand(2)), OperatorNode(Tan, OperandNode(IntegerOperand(50)), OperandNode(DecimalOperand(0.0)))), OperandNode(IntegerOperand(22))))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should fail on 6*/3" ignore {

      val input = List(Start, Literal("6"), Ident("*"), Ident("/"), Literal("3"), End)
      val expectation = Left(ParsingFailure("missing operand", input, Location(2)))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should fail on 6-/3" ignore {

      val input = List(Start, Literal("6"), Ident("-"), Ident("/"), Literal("3"), End)
      val expectation = Left(ParsingFailure("missing operand", input, Location(2)))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should parse 8" in {

      val input = List(Start, Literal("8"), End)
      val expectation = Right(OperandNode(IntegerOperand(8)))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should fail on *8" ignore {

      val input = List(Start, Ident("*"), Literal("8"), End)
      val expectation = Left(ParsingFailure("missing operand", input, Location(1)))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should fail on *2+2" ignore {

      val input = List(Start, Ident("*"), Literal("8"), Ident("+"), Literal("2"), End)
      val expectation = Left(ParsingFailure("missing operand", input, Location(0)))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should fail on 3+5*" ignore {

      val input = List(Start, Literal("3"), Ident("+"), Literal("5"), Ident("*"), End)
      val expectation = Left(ParsingFailure("missing operand", input, Location(3)))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should fail on sin" ignore {

      val input = List(Start, Ident("sin"), End)
      val expectation = Left(ParsingFailure("missing operand", input, Location(0)))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should fail on /" ignore {

      val input = List(Start, Ident("/"), End)
      val expectation = Left(ParsingFailure("missing operand", input, Location(0)))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should fail on ()" ignore {

      val input = List(Start, Parenthesis(Open), Parenthesis(Close), End)
      val expectation = Left(ParsingFailure("", input, Location(1)))

      val result = Parser.parse(input)

      assert(result == expectation)
    }


    "should parse 6.5-8" in {

      val input = List(Start, FloatingPointLiteral("6.5"), Ident("-"), Literal("8"), End)
      val expectation = Right(OperatorNode(Sub, OperandNode(DecimalOperand(6.5)), OperandNode(IntegerOperand(8))))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should fail on 5+" ignore {

      val input = List(Start, Literal("5"), Ident("+"), End)
      val expectation = Left(ParsingFailure("missing operand", input, Location(1)))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should fail on +." in {

      val input = List(Start, FloatingPointLiteral("+."), End)
      val expectation = Left(ParsingFailure("Literal is not a decimal number: +.", input, Location(1)))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should fail on 44()" ignore {

      val input = List(Start, Literal("44"), Ident("*"), Parenthesis(Open), Parenthesis(Close), End)
      val expectation = Left(ParsingFailure("", input, Location(1)))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should parse pi" in {

      val input = List(Start, ConstantLiteral("pi"), End)
      val expectation = Right(OperandNode(DecimalOperand(Math.PI)))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should fail on sin()" in {

      val input = List(Start, Ident("sin"), Parenthesis(Open), Parenthesis(Close), End)
      val expectation = Left(ParsingFailure("missing operand", input, Location(5)))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should parse e" in {

      val input = List(Start, ConstantLiteral("e"), End)
      val expectation = Right(OperandNode(DecimalOperand(Math.E)))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should parse e^8" in {

      val input = List(Start, ConstantLiteral("e"), Ident("^"), Literal("8"), End)
      val expectation = Right(OperatorNode(Pow, OperandNode(DecimalOperand(Math.E)), OperandNode(IntegerOperand(8))))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should parse 4!" in {

      val input = List(Start, Literal("4"), Ident("!"), End)
      val expectation = Right(OperatorNode(Fac, OperandNode(IntegerOperand(4)), OperandNode(DecimalOperand(0.0))))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should parse sqrt(4)" in {

      val input = List(Start, Ident("sqrt"), Parenthesis(Open), Literal("4"), Parenthesis(Close), End)
      val expectation = Right(OperatorNode(Sqrt, OperandNode(IntegerOperand(4)), OperandNode(DecimalOperand(0.0))))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should fail on 5.45!" ignore {

      val input = List(Start, FloatingPointLiteral("5.45"), Ident("!"), End)
      val expectation = Left(ParsingFailure("cannot compute faculty of floating literal", input, Location(2)))

      val result = Parser.parse(input)

      assert(result == expectation)
    }
  }
}
