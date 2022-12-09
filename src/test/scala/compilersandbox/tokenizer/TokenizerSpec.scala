package compilersandbox.tokenizer

import Tokens.{End, FloatingPointLiteral, Ident, Literal, Parenthesis, Start}
import Tokens.ParenthesisKind.{Close, Open}
import compilersandbox.tokenizer.Tokenizer.TokenizerFailure
import compilersandbox.util.Location
import org.junit.runner.RunWith
import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TokenizerSpec extends AnyFreeSpec {

  implicit val doubleEq: Equality[Double] = TolerantNumerics.tolerantDoubleEquality(1e-4f)

  "A Tokenizer" - {

    "should tokenize 1" in {

      val expected = Right(List(Start, Literal("1"), End))
      val result = Tokenizer.tokenize("1")

      assert(result == expected)
    }

    "should tokenize 12" in {

      val expected = Right(List(Start, Literal("12"), End))
      val result = Tokenizer.tokenize("12")

      assert(result == expected)
    }

    "should tokenize a" in {

      val expected = Right(List(Start, Ident("a"), End))
      val result = Tokenizer.tokenize("a")

      assert(result == expected)
    }

    "should tokenize ab" in {

      val expected = Right(List(Start, Ident("ab"), End))
      val result = Tokenizer.tokenize("ab")

      assert(result == expected)
    }

    "should tokenize +" in {

      val expected = Right(List(Start, Literal("+"), End))
      val result = Tokenizer.tokenize("+")

      assert(result == expected)
    }

    "should tokenize 1+2" in {

      val expected = Right(List(Start, Literal("1"), Ident("+"), Literal("2"), End))
      val result = Tokenizer.tokenize("1+2")

      assert(result == expected)
    }

    "should tokenize -" in {

      val expected = Right(List(Start, Literal("-"), End))
      val result = Tokenizer.tokenize("-")

      assert(result == expected)
    }

    "should tokenize 1-2" in {

      val expected = Right(List(Start, Literal("1"), Ident("-"), Literal("2"), End))
      val result = Tokenizer.tokenize("1-2")

      assert(result == expected)
    }

    "should tokenize 1--2" in {

      val expected = Right(List(Start, Literal("1"), Ident("-"), Literal("-2"), End))
      val result = Tokenizer.tokenize("1--2")

      assert(result == expected)
    }

    "should tokenize *" in {

      val expected = Right(List(Start, Ident("*"), End))
      val result = Tokenizer.tokenize("*")

      assert(result == expected)
    }

    "should tokenize 2/3" in {

      val expected = Right(List(Start, Literal("2"), Ident("/"), Literal("3"), End))
      val result = Tokenizer.tokenize("2/3")

      assert(result == expected)
    }

    "should tokenize 1*/2" in {

      val expected = Right(List(Start, Literal("1"), Ident("*"), Ident("/"), Literal("2"), End))
      val result = Tokenizer.tokenize("1*/2")

      assert(result == expected)
    }

    "should tokenize 1*cos2" in {

      val expected = Right(List(Start, Literal("1"), Ident("*"), Ident("cos"), Literal("2"), End))
      val result = Tokenizer.tokenize("1*cos2")

      assert(result == expected)
    }

    "should tokenize (3/-2)" in {

      val expected = Right(List(Start, Parenthesis(Open), Literal("3"), Ident("/"), Literal("-2"), Parenthesis(Close), End))
      val result = Tokenizer.tokenize("(3/-2)")

      assert(result == expected)
    }

    "should tokenize 2(1*3)-4" in {

      val expected = Right(List(Start, Literal("2"), Parenthesis(Open), Literal("1"), Ident("*"), Literal("3"), Parenthesis(Close), Ident("-"), Literal("4"), End))
      val result = Tokenizer.tokenize("2(1*3)-4")

      assert(result == expected)
    }

    "should tokenize (-1+22)" in {

      val expected = Right(List(Start, Parenthesis(Open), Literal("-1"), Ident("+"), Literal("22"), Parenthesis(Close), End))
      val result = Tokenizer.tokenize("(-1+22)")

      assert(result == expected)
    }

    "should tokenize 1.2" in {

      val expected = Right(List(Start, FloatingPointLiteral("1.2"), End))
      val result = Tokenizer.tokenize("1.2")

      assert(result == expected)
    }

    "should tokenize cos()" in {

      val expected = Right(List(Start, Ident("cos"), Parenthesis(Open), Parenthesis(Close), End))
      val result = Tokenizer.tokenize("cos()")

      assert(result == expected)
    }

    "should tokenize 6ia12-*/6)" in {

      val expected = Right(List(Start, Literal("6"), Ident("ia"), Literal("12"), Ident("-"), Ident("*"), Ident("/"), Literal("6"), Parenthesis(Close), End))
      val result = Tokenizer.tokenize("6ia12-*/6)")

      assert(result == expected)
    }

    "should fail on ." in {

      val input = "."
      val expected = Left(TokenizerFailure("unexpected token", input, Location(0)))
      val result = Tokenizer.tokenize(input)

      assert(result == expected)
    }

    "should fail on 8+7.8.*3" in {

      val input = "8+7.8.*3"
      val expected = Left(TokenizerFailure("unexpected token", input, Location(5)))
      val result = Tokenizer.tokenize(input)

      assert(result == expected)
    }

    "should fail on 4++" ignore {

      val input = "4++"
      val expected = Left(TokenizerFailure("unexpected token", input, Location(2)))
      val result = Tokenizer.tokenize(input)

      assert(result == expected)
    }

    "should tokenize 9 +1" in {

      val expected = Right(List(Start, Literal("9"), Ident("+"), Literal("1"), End))
      val result = Tokenizer.tokenize("9 +1")

      assert(result == expected)
    }

    "should tokenize SIN()" in {

      val expected = Right(List(Start, Ident("sin"), Parenthesis(Open), Parenthesis(Close), End))
      val result = Tokenizer.tokenize("SIN()")

      assert(result == expected)
    }

    "should tokenize sinpi" in {

      val expected = Right(List(Start, Ident("sin"), Ident("pi"), End))
      val result = Tokenizer.tokenize("sinpi")

      assert(result == expected)
    }


    "should tokenize esin" in {

      val expected = Right(List(Start, Ident("e"), Ident("sin"), End))
      val result = Tokenizer.tokenize("esin")

      assert(result == expected)
    }

    "should tokenize pi+" in {

      val expected = Right(List(Start, Ident("pi"), Ident("+"), End))
      val result = Tokenizer.tokenize("pi+")

      assert(result == expected)
    }
  }
}
