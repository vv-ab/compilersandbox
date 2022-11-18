package compilersandbox

import compilersandbox.parser.Parser
import compilersandbox.tokenizer.{Preprocessor, Tokenizer}
import org.junit.runner.RunWith
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.junit.JUnitRunner

import scala.collection.mutable

@RunWith(classOf[JUnitRunner])
class IntegrationSpec extends AnyFreeSpec {

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
            val result = tree.compute()
            assert(result == 8)
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
            val result = tree.compute()
            assert(result == 0)
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
            val result = tree.compute()
            assert(result == -50)
        }

    }
  }
}
