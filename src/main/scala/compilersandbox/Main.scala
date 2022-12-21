package compilersandbox

import compilersandbox.parser.Parser
import compilersandbox.tokenizer.{Preprocessor, Tokenizer}
import Tokenizer.TokenizerFailure
import compilersandbox.analyser.Analyser
import compilersandbox.compute.Compute

import scala.collection.mutable

@main
def main(): Unit = {
  print("Enter expression: ")
  val input = Console.in.readLine()

  input.evaluate match {
    case Left(failures) =>
      println(failures.head.message)
    case Right(value) =>
      val text = value.fold(_.toString, _.toString)
      println(s"Result: $text")
  }
}

def makeErrorMessage(failure: TokenizerFailure): String = {

  val arrow = (0 until failure.location.value).foldLeft("^")({ (result, _) => s"-$result" })
  s"""
     |${failure.input}
     |$arrow ${failure.message}
     |""".stripMargin
}
