package compilersandbox.tokenizer

import compilersandbox.tokenizer.TokenizerV2.{End, FloatingPointLiteral, Ident, Literal, Parenthesis, Start, tokenize}
import compilersandbox.tokenizer.TokenizerV2.ParenthesisKind.{Close, Open}
import org.junit.runner.RunWith
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TokenizerV2Spec extends AnyFreeSpec {

  "A Tokenizer" - {

    "should tokenize 1" in {

      val expected = Right(List(Start, Literal("1"), End))
      val result = TokenizerV2.tokenize("1")

      assert(result == expected)
    }

    "should tokenize 12" in {

      val expected = Right(List(Start, Literal("12"), End))
      val result = TokenizerV2.tokenize("12")

      assert(result == expected)
    }

    "should tokenize a" in {

      val expected = Right(List(Start, Ident("a"), End))
      val result = TokenizerV2.tokenize("a")

      assert(result == expected)
    }

    "should tokenize ab" in {

      val expected = Right(List(Start, Ident("ab"), End))
      val result = TokenizerV2.tokenize("ab")

      assert(result == expected)
    }

    "should tokenize +" in {

      val expected = Right(List(Start, Literal("+"), End))
      val result = TokenizerV2.tokenize("+")

      assert(result == expected)
    }

    "should tokenize 1+2" in {

      val expected = Right(List(Start, Literal("1"), Ident("+"), Literal("2"), End))
      val result = TokenizerV2.tokenize("1+2")

      assert(result == expected)
    }

    "should tokenize -" in {

      val expected = Right(List(Start, Literal("-"), End))
      val result = TokenizerV2.tokenize("-")

      assert(result == expected)
    }

    "should tokenize 1-2" in {

      val expected = Right(List(Start, Literal("1"), Ident("-"), Literal("2"), End))
      val result = TokenizerV2.tokenize("1-2")

      assert(result == expected)
    }

    "should tokenize 1--2" in {

      val expected = Right(List(Start, Literal("1"), Ident("-"), Literal("-2"), End))
      val result = TokenizerV2.tokenize("1--2")

      assert(result == expected)
    }

    "should tokenize *" in {

      val expected = Right(List(Start, Ident("*"), End))
      val result = TokenizerV2.tokenize("*")

      assert(result == expected)
    }

    "should tokenize 2/3" in {

      val expected = Right(List(Start, Literal("2"), Ident("/"), Literal("3"), End))
      val result = TokenizerV2.tokenize("2/3")

      assert(result == expected)
    }

    "should tokenize 1*/2" in {

      val expected = Right(List(Start, Literal("1"), Ident("*"), Ident("/"), Literal("2"), End))
      val result = TokenizerV2.tokenize("1*/2")

      assert(result == expected)
    }

    "should tokenize 1*cos2" in {

      val expected = Right(List(Start, Literal("1"), Ident("*"), Ident("cos"), Literal("2"), End))
      val result = TokenizerV2.tokenize("1*cos2")

      assert(result == expected)
    }

    "should tokenize (3/-2)" in {

      val expected = Right(List(Start, Parenthesis(Open), Literal("3"), Ident("/"), Literal("-2"), Parenthesis(Close), End))
      val result = TokenizerV2.tokenize("(3/-2)")

      assert(result == expected)
    }

    "should tokenize 2(1*3)-4" in {

      val expected = Right(List(Start, Literal("2"), Parenthesis(Open), Literal("1"), Ident("*"), Literal("3"), Parenthesis(Close), Ident("-"), Literal("4"), End))
      val result = TokenizerV2.tokenize("2(1*3)-4")

      assert(result == expected)
    }

    "should tokenize (-1+22)" in {

      val expected = Right(List(Start, Parenthesis(Open), Literal("-1"), Ident("+"), Literal("22"), Parenthesis(Close), End))
      val result = TokenizerV2.tokenize("(-1+22)")

      assert(result == expected)
    }

    "should tokenize 1.2" in {

      val expected = Right(List(Start, FloatingPointLiteral("1.2"), End))
      val result = TokenizerV2.tokenize("1.2")

      assert(result == expected)
    }

    "should tokenize cos()" in {

      val expected = Right(List(Start, Ident("cos"), Parenthesis(Open), Parenthesis(Close), End))
      val result = TokenizerV2.tokenize("cos()")

      assert(result == expected)
    }

    "should tokenize 6ia12-*/6)" in {

      val expected = Right(List(Start, Literal("6"), Ident("ia"), Literal("12"), Ident("-"), Ident("*"), Ident("/"), Literal("6"), Parenthesis(Close), End))
      val result = TokenizerV2.tokenize("6ia12-*/6)")

      assert(result == expected)
    }
  }
}
