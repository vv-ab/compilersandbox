package compilersandbox

import compilersandbox.model.*
import compilersandbox.parser.Parser

import scala.collection.mutable

@main
def main(): Unit = {
  print("Enter expression: ")
  val input = Console.in.readLine()
  val tree = Parser.parse(input, mutable.Stack.empty, mutable.Stack.empty)
  val result = tree.compute()
  println(s"Result: $result")
}
