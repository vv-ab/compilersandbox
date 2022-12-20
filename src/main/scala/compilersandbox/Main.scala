package compilersandbox

import compilersandbox.parser.Parser
import compilersandbox.tokenizer.{Preprocessor, Tokenizer}
import Tokenizer.TokenizerFailure
import compilersandbox.compute.Compute

import scala.collection.mutable

@main
def main(): Unit = {
  print("Enter expression: ")
  val input = Console.in.readLine()
  val tokens = Tokenizer.tokenize(input)
  tokens match {
    case Left(failure) =>
      println(makeErrorMessage(failure))

    case Right(tokens) =>
      val preprocessedTokens = Preprocessor.preprocess(tokens, List.empty)
      val tree = Parser.parse(preprocessedTokens)
      tree match {
        case Left(failure) =>
          println(s"Error: ${failure.message}")
        case Right(tree) =>
          val result: Unit = Compute.compute(tree) match {
            case Left(value) =>
              println(s"Error: $value")
            case Right(value) =>
              value match {
                case Left(value) =>
                  println(s"Result: $value")
                case Right(value) =>
                  println(s"Result: $value")
              }
          }
      }
  }
}

def makeErrorMessage(failure: TokenizerFailure): String = {

  val arrow = (0 until failure.location.value).foldLeft("^")({ (result, _) => s"-$result" })
  s"""
     |${failure.input}
     |$arrow ${failure.message}
     |""".stripMargin
}
