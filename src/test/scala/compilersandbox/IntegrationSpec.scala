package compilersandbox

import compilersandbox.compute.Compute
import compilersandbox.parser.Parser
import compilersandbox.tokenizer.{Preprocessor, Tokenizer}
import org.junit.runner.RunWith
import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.Inside
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.junit.JUnitRunner

import scala.collection.mutable

@RunWith(classOf[JUnitRunner])
class IntegrationSpec extends AnyFreeSpec with Inside {

  implicit val doubleEq: Equality[Double] = TolerantNumerics.tolerantDoubleEquality(1e-4f)

  "Should compute 5+3" in {

    val input = "5+3"
    val tokens = Tokenizer.tokenize(input)
    tokens match {
      case Left(failure) =>
        fail(failure.message)
      case Right(tokens) =>
        val preprocessedTokens = Preprocessor.preprocess(tokens, List.empty)
        val tree = Parser.parse(preprocessedTokens)
        tree match {
          case Left(failure) =>
            fail(failure.message)
          case Right(tree) =>
            val result = Compute.compute(tree)
            inside(result) { case Right(Left(value)) =>
              assert(value === 8)
            }
        }
    }
  }

  "Should compute 2(88/22)-2^3" in {

    val input = "2(88/22)-2^3"
    val tokens = Tokenizer.tokenize(input)
    tokens match {
      case Left(failure) =>
        fail(failure.message)
      case Right(tokens) =>
        val preprocessedTokens = Preprocessor.preprocess(tokens, List.empty)
        val tree = Parser.parse(preprocessedTokens)
        tree match {
          case Left(failure) =>
            fail(failure.message)
          case Right(tree) =>
            val result = Compute.compute(tree)
            inside(result) { case Right(Right(value)) =>
              assert(value === 0.0)
            }
        }
    }
  }

  "Should compute -4*cos(0)(7.5--5)" in {

    val input = "-4*cos(0)(7.5--5)"
    val tokens = Tokenizer.tokenize(input)
    tokens match {
      case Left(failure) =>
        fail(failure.message)
      case Right(tokens) =>
        val preprocessedTokens = Preprocessor.preprocess(tokens, List.empty)
        val tree = Parser.parse(preprocessedTokens)
        tree match {
          case Left(failure) =>
            fail(failure.message)
          case Right(tree) =>
            val result = Compute.compute(tree)
            inside(result) { case Right(Right(value)) =>
              assert(value === -50.0)
            }
        }

    }
  }

  "should compute sin(30)" in {

    val input = "sin(30)"
    val tokens = Tokenizer.tokenize(input)
    tokens match {
      case Left(failure) =>
        fail(failure.message)
      case Right(tokens) =>
        val preprocessedTokens = Preprocessor.preprocess(tokens, List.empty)
        val tree = Parser.parse(preprocessedTokens)
        tree match {
          case Left(failure) =>
            fail(failure.message)
          case Right(tree) =>
            val result = Compute.compute(tree)
            inside(result) { case Right(Right(value)) =>
              assert(value === 0.50)
            }
        }

    }
  }

  "should compute SIN(30)" in {

    val input = "SIN(30)"
    val tokens = Tokenizer.tokenize(input)
    tokens match {
      case Left(failure) =>
        fail(failure.message)
      case Right(tokens) =>
        val preprocessedTokens = Preprocessor.preprocess(tokens, List.empty)
        val tree = Parser.parse(preprocessedTokens)
        tree match {
          case Left(failure) =>
            fail(failure.message)
          case Right(tree) =>
            val result = Compute.compute(tree)
            inside(result) { case Right(Right(value)) =>
              assert(value === 0.50)
            }
        }

    }
  }

  "should fail on 9++" in {

    val input = "9++"
    val tokens = Tokenizer.tokenize(input)
    tokens match {
      case Left(failure) =>
        fail(failure.message)
      case Right(tokens) =>
        val preprocessedTokens = Preprocessor.preprocess(tokens, List.empty)
        val tree = Parser.parse(preprocessedTokens)
        assert(tree.isLeft)
    }
  }

  "should compute expressions with constants" in {

    val input = "3pi"
    val tokens = Tokenizer.tokenize(input)
    tokens match {
      case Left(failure) =>
        fail(failure.message)
      case Right(tokens) =>
        val preprocessedTokens = Preprocessor.preprocess(tokens, List.empty)
        val tree = Parser.parse(preprocessedTokens)
        tree match {
          case Left(failure) =>
            fail(failure.message)
          case Right(tree) =>
            val result = Compute.compute(tree)
            inside(result) { case Right(Right(value)) =>
              assert(value === 9.42477796077)
            }
        }
    }
  }

  "should compute 3!+33" in {

    val input = "3!+33"
    val tokens = Tokenizer.tokenize(input)
    tokens match {
      case Left(failure) =>
        fail(failure.message)
      case Right(tokens) =>
        val preprocessedTokens = Preprocessor.preprocess(tokens, List.empty)
        val tree = Parser.parse(preprocessedTokens)
        tree match {
          case Left(failure) =>
            fail(failure.message)
          case Right(tree) =>
            val result = Compute.compute(tree)
            inside(result) { case Right(Left(value)) =>
              assert(value === 39)
            }
        }
    }
  }

  "should compute 3!/2" in {

    val input = "3!/2"
    val tokens = Tokenizer.tokenize(input)
    tokens match {
      case Left(failure) =>
        fail(failure.message)
      case Right(tokens) =>
        val preprocessedTokens = Preprocessor.preprocess(tokens, List.empty)
        val tree = Parser.parse(preprocessedTokens)
        tree match {
          case Left(failure) =>
            fail(failure.message)
          case Right(tree) =>
            val result = Compute.compute(tree)
            inside(result) { case Right(Right(value)) =>
              assert(value === 3.0)
            }
        }
    }
  }

  "should compute sqrt(4)" in {

    val input = "sqrt(4)"
    val tokens = Tokenizer.tokenize(input)
    tokens match {
      case Left(failure) =>
        fail(failure.message)
      case Right(tokens) =>
        val preprocessedTokens = Preprocessor.preprocess(tokens, List.empty)
        val tree = Parser.parse(preprocessedTokens)
        tree match {
          case Left(failure) =>
            fail(failure.message)
          case Right(tree) =>
            val result = Compute.compute(tree)
            inside(result) { case Right(Right(value)) =>
              assert(value === 2.0)
            }
        }
    }
  }

  "should compute sqrt(36)/2" in {

    val input = "sqrt(36)/2"
    val tokens = Tokenizer.tokenize(input)
    tokens match {
      case Left(failure) =>
        fail(failure.message)
      case Right(tokens) =>
        val preprocessedTokens = Preprocessor.preprocess(tokens, List.empty)
        val tree = Parser.parse(preprocessedTokens)
        tree match {
          case Left(failure) =>
            fail(failure.message)
          case Right(tree) =>
            val result = Compute.compute(tree)
            inside(result) { case Right(Right(value)) =>
              assert(value === 3.0)
            }
        }
    }
  }
}
