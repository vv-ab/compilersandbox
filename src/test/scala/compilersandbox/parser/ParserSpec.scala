package compilersandbox.parser

import compilersandbox.tokenizer.Tokens.ParenthesisKind.{Close, Open}
import compilersandbox.tokenizer.Tokens.{End, FloatingPointLiteral, Ident, Literal, Parenthesis, ParenthesisKind, Start, Token}
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
      val expectation = Right(OperatorNode(Add, OperandNode(Operand(5)), OperandNode(Operand(3))))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should parse an expression with subtraction" in {

      val input = List(Start, Literal("7"), Ident("-"), Literal("2"), End)
      val expectation = Right(OperatorNode(Sub, OperandNode(Operand(7)), OperandNode(Operand(2))))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should parse 8*2" in {

      val input = List(Start, Literal("8"), Ident("*"), Literal("2"), End)
      val expectation = Right(OperatorNode(Mul, OperandNode(Operand(8)), OperandNode(Operand(2))))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should parse 2*(1+1)" in {

      val input = List(Start, Literal("2"), Ident("*"), Parenthesis(Open), Literal("1"), Ident("+"), Literal("1"), Parenthesis(Close), End)
      val expectation = Right(OperatorNode(Mul, OperandNode(Operand(2)), OperatorNode(Add, OperandNode(Operand(1)), OperandNode(Operand(1)))))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should parse 2*(1*(3+4))" in {

      val input = List(Start, Literal("2"), Ident("*"), Parenthesis(Open), Literal("1"), Ident("*"), Parenthesis(Open), Literal("3"), Ident("+"), Literal("4"), Parenthesis(Close), Parenthesis(Close), End)
      val expectation = Right(OperatorNode(Mul, OperandNode(Operand(2)), OperatorNode(Mul, OperandNode(Operand(1)), OperatorNode(Add, OperandNode(Operand(3)), OperandNode(Operand(4))))))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should parse an expression with division" in {

      val input = List(Start, Literal("4"), Ident("/"), Literal("2"), End)
      val expectation = Right(OperatorNode(Div, OperandNode(Operand(4)), OperandNode(Operand(2))))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should parse expressions with parentheses" in {

      val input = List(Start, Parenthesis(Open), Literal("8"), Ident("-"), Literal("6"), Parenthesis(Close), End)
      val expectation = Right(OperatorNode(Sub, OperandNode(Operand(8)), OperandNode(Operand(6))))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should parse expressions with power" in {

      val input = List(Start, Literal("3"), Ident("^"), Literal("3"), End)
      val expectation = Right(OperatorNode(Pow, OperandNode(Operand(3)), OperandNode(Operand(3))))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should parse 2*(2+2^2)" in {

      val input = List(Start, Literal("2"), Ident("*"), Parenthesis(Open), Literal("2"), Ident("+"), Literal("2"), Ident("^"), Literal("2"), Parenthesis(Close), End)
      val expectation = Right(OperatorNode(Mul, OperandNode(Operand(2)), OperatorNode(Add, OperandNode(Operand(2)), OperatorNode(Pow, OperandNode(Operand(2)), OperandNode(Operand(2))))))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should parse 2*(2+(2^2))" in {

      val input = List(Start, Literal("2"), Ident("*"), Parenthesis(Open), Literal("2"), Ident("+"), Parenthesis(Open), Literal("2"), Ident("^"), Literal("2"), Parenthesis(Close), Parenthesis(Close), End)
      val expectation = Right(OperatorNode(Mul, OperandNode(Operand(2)), OperatorNode(Add, OperandNode(Operand(2)), OperatorNode(Pow, OperandNode(Operand(2)), OperandNode(Operand(2))))))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should parse trigonometric expressions (sine)" in {

      val input = List(Start, Ident("sin"), Parenthesis(Open), Literal("90"), Parenthesis(Close), End)
      val expectation = Right(OperatorNode(Sin, OperandNode(Operand(90)), OperandNode(Operand(0))))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should parse 2*(sin(90))-22" in {

      val input = List(Start, Literal("2"), Ident("*"), Parenthesis(Open), Ident("sin"), Parenthesis(Open), Literal("90"), Parenthesis(Close), Parenthesis(Close), Ident("-"), Literal("22"), End)
      val expectation = Right(OperatorNode(Sub, OperatorNode(Mul, OperandNode(Operand(2)), OperatorNode(Sin, OperandNode(Operand(90)), OperandNode(Operand(0)))), OperandNode(Operand(22))))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should parse trigonometric expressions (cosine)" in {

      val input = List(Start, Ident("cos"), Parenthesis(Open), Literal("0"), Parenthesis(Close), End)
      val expectation = Right(OperatorNode(Cos, OperandNode(Operand(0)), OperandNode(Operand(0))))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should parse 2*(cos(0))-22" in {

      val input = List(Start, Literal("2"), Ident("*"), Parenthesis(Open), Ident("cos"), Parenthesis(Open), Literal("0"), Parenthesis(Close), Parenthesis(Close), Ident("-"), Literal("22"), End)
      val expectation = Right(OperatorNode(Sub, OperatorNode(Mul, OperandNode(Operand(2)), OperatorNode(Cos, OperandNode(Operand(0)), OperandNode(Operand(0)))), OperandNode(Operand(22))))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should parse trigonometric expressions (tangents)" in {

      val input = List(Start, Ident("tan"), Parenthesis(Open), Literal("50"), Parenthesis(Close), End)
      val expectation = Right(OperatorNode(Tan, OperandNode(Operand(50)), OperandNode(Operand(0))))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should parse 2*(tan(50))-22" in {

      val input = List(Start, Literal("2"), Ident("*"), Parenthesis(Open), Ident("tan"), Parenthesis(Open), Literal("50"), Parenthesis(Close), Parenthesis(Close), Ident("-"), Literal("22"), End)
      val expectation = Right(OperatorNode(Sub, OperatorNode(Mul, OperandNode(Operand(2)), OperatorNode(Tan, OperandNode(Operand(50)), OperandNode(Operand(0)))), OperandNode(Operand(22))))

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
      val expectation = Right(OperandNode(Operand(8)))

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

      val input = List(Start, FloatingPointLiteral("6.5"), Ident("-"), Literal("8"))
      val expectation = Right(OperatorNode(Sub, OperandNode(Operand(6.5)), OperandNode(Operand(8))))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should fail on 5+" ignore {

      val input = List(Start, Literal("5"), Ident("+"))
      val expectation = Left(ParsingFailure("missing operand", input, Location(1)))

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

      val input = List(Start, FloatingPointLiteral("pi"), End)
      val expectation = Right(OperandNode(Operand(Math.PI)))

      val result = Parser.parse(input)

      assert(result == expectation)
    }

    "should fail on sin()" ignore {

      val input = List(Start, Ident("sin"), Parenthesis(Open), Parenthesis(Close), End)
      val expectation = Left(ParsingFailure("missing operand", input, Location(5)))

      val result = Parser.parse(input)

      assert(result == expectation)
    }
  }
}
