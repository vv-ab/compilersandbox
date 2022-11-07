package compilersandbox

import compilersandbox.model.*
import compilersandbox.parser.Parser
import compilersandbox.tokenizer.{Start, Tokenizer}

import scala.collection.mutable

@main
def main(): Unit = {
  print("Enter expression: ")
  val input = Console.in.readLine()
  val tokens = Tokenizer.tokenize(input, Start, List.empty)
  val tree = Parser.parse(tokens, mutable.Stack.empty, mutable.Stack.empty)
  val result = tree.compute()
  println(s"Result: $result")
}
