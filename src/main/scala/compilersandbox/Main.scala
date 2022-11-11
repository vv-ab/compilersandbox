package compilersandbox

import compilersandbox.parser.Parser
import compilersandbox.tokenizer.{Preprocessor, Start, Tokenizer}

import scala.collection.mutable

@main
def main(): Unit = {
  print("Enter expression: ")
  val input = Console.in.readLine()
  val tokens = Tokenizer.tokenize(input, Start, List.empty)
  tokens match {
    case Left(failure) =>
      ???
    case Right(tokens) =>
      val processedTokens = Preprocessor.preprocess(tokens, List.empty)
      val tree = Parser.parse(processedTokens, mutable.Stack.empty, mutable.Stack.empty)
      tree match {
        case Left(failure) =>
          ???
        case Right(tree) =>
          val result = tree.compute()
          println(s"Result: $result")
      }
  }
}
