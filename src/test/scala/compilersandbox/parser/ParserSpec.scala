package compilersandbox.parser

import compilersandbox.tokenizer.ParenthesisKind.{Close, Open}
import compilersandbox.tokenizer.{End, Number, Operator, Parenthesis, ParenthesisKind, Start}
import org.scalatest.freespec.AnyFreeSpec

import scala.collection.mutable

class ParserSpec extends AnyFreeSpec {

  "A Parser " - {

    "should parse 5+3" in {

      val input = List(Start, Number("5"), Operator("+"), Number("3"), End)
      val expectation = OperatorNode(Add, OperandNode(Operand(5)), OperandNode(Operand(3)))

      val result = Parser.parse(input, mutable.Stack.empty, mutable.Stack.empty)

      assert(result == expectation)
    }

    "should parse an expression with subraction" in {

      val input = List(Start, Number("7"), Operator("-"), Number("2"), End)
      val expectation = OperatorNode(Sub, OperandNode(Operand(7)), OperandNode(Operand(2)))

      val result = Parser.parse(input, mutable.Stack.empty, mutable.Stack.empty)

      assert(result == expectation)
    }

    "should parse 8*2" in {

      val input = List(Start, Number("8"), Operator("*"), Number("2"), End)
      val expectation = OperatorNode(Mul, OperandNode(Operand(8)), OperandNode(Operand(2)))

      val result = Parser.parse(input, mutable.Stack.empty, mutable.Stack.empty)

      assert(result == expectation)
    }

    "should parse an expression with division" in {

      val input = List(Start, Number("4"), Operator("/"), Number("2"), End)
      val expectation = OperatorNode(Div, OperandNode(Operand(4)), OperandNode(Operand(2)))

      val result = Parser.parse(input, mutable.Stack.empty, mutable.Stack.empty)

      assert(result == expectation)
    }

    "should parse expressions with parenthese" in {

      val input = List(Start, Parenthesis(Open), Number("8"), Operator("-"), Number("6"), Parenthesis(Close), End)
      val expectation = OperatorNode(Sub, OperandNode(Operand(8)), OperandNode(Operand(6)))

      val result = Parser.parse(input, mutable.Stack.empty, mutable.Stack.empty)

      assert(result == expectation)
    }
  }
}