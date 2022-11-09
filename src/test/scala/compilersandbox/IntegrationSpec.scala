package compilersandbox

import compilersandbox.parser.Parser
import compilersandbox.tokenizer.{Preprocessor, Start, Tokenizer}
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.mutable

class IntegrationSpec extends AnyFreeSpec {

  "Should compute 5+3" in {

    val input = "5+3"
    val tokens = Tokenizer.tokenize(input, Start, List.empty)
    val processedTokens = Preprocessor.preprocess(tokens, List.empty)
    val tree = Parser.parse(processedTokens, mutable.Stack.empty, mutable.Stack.empty)
    val result = tree.compute()

    assert(result == 8)
  }

  "Should compute 2(88/22)-2^3" in {

    val input = "2(88/22)-2^3"
    val tokens = Tokenizer.tokenize(input, Start, List.empty)
    val processedTokens = Preprocessor.preprocess(tokens, List.empty)
    val tree = Parser.parse(processedTokens, mutable.Stack.empty, mutable.Stack.empty)
    val result = tree.compute()

    assert(result == 0)
  }

  "Should compute 4*cos(0)(7-5)" in {

    val input = "4*cos(0)(7-5)"
    val tokens = Tokenizer.tokenize(input, Start, List.empty)
    val processedTokens = Preprocessor.preprocess(tokens, List.empty)
    val tree = Parser.parse(processedTokens, mutable.Stack.empty, mutable.Stack.empty)
    val result = tree.compute()

    assert(result == 8)
  }
}
