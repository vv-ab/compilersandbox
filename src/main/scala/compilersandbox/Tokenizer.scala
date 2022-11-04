package compilersandbox

import compilersandbox.ParenthesisKind.{Close, Open}
import compilersandbox.model.*

import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.collection.mutable

object Tokenizer {

  def tokenize(input: Seq[Char], current: Token, tokens: List[Token]): List[Token] = {

    input.headOption match {
      case Some(character) =>
        character match {
          case '+' | '-'   =>
            current match {
              case _: Number | Parenthesis(Close) =>
                tokenize(input.tail, Operator(s"$character"), tokens :+ current)
              case Parenthesis(Open) | Start =>
                tokenize(input.tail, Number(s"$character"), tokens :+ current)
              case _: Operator => // error
                ???
            }
          case '*' | '/' =>
            current match {
              case _: Operator | Start | Parenthesis(Open) => // error
                ???
              case _: Number | Parenthesis(Close) =>
                tokenize(input.tail, Operator(s"$character"), tokens :+ current)
            }
          case '(' =>
            current match {
              case Start | _: Operator | Parenthesis(Open) =>
                tokenize(input.tail, Parenthesis(Open), tokens :+ current)
              case _: Number | Parenthesis(Close) => // error
                ???
            }
          case ')' =>
            current match {
              case Start | _: Operator | Parenthesis(Open) => // error
                ???
              case _: Number | Parenthesis(Close) =>
                tokenize(input.tail, Parenthesis(Close), tokens :+ current)
            }
          case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
            current match
              case Start | Parenthesis(Open) | _: Operator =>
                tokenize(input.tail, Number(s"$character"), tokens :+ current)
              case Number(value) =>
                tokenize(input.tail, Number(s"$value$character"), tokens)
              case Parenthesis(Close) => // error
                ???
          case _ =>
            ???
        }
      case None =>
        (tokens :+ current) :+ End

    }
  }
}

sealed trait Token {}


case class Operator(value: String) extends Token

case object Start extends Token

case object End extends Token

case class Number(value: String) extends Token

case class Parenthesis(value: ParenthesisKind) extends Token

enum ParenthesisKind {
  case Open, Close
}

@main
def tokenizerMain(): Unit = {

  val input1 = "59+3"
  val tokens1 = Tokenizer.tokenize(input1, Start, List.empty)
  println(s"$input1: $tokens1")

  val input2 = "(5+3)"
  val tokens2 = Tokenizer.tokenize(input2, Start, List.empty)
  println(s"$input2: $tokens2")

  val input3 = "5*(8-1)"
  val tokens3 = Tokenizer.tokenize(input3, Start, List.empty)
  println(s"$input3: $tokens3")

  val input4 = "(-8+1)"
  val tokens4 = Tokenizer.tokenize(input4, Start, List.empty)
  println(s"$input4: $tokens4")

  val input5 = "-8+1"
  val tokens5 = Tokenizer.tokenize(input5, Start, List.empty)
  println(s"$input5: $tokens5")

  val input6 = "-84+12"
  val tokens6 = Tokenizer.tokenize(input6, Start, List.empty)
  println(s"$input6: $tokens6")

  val input7 = "(100-84)+12"
  val tokens7 = Tokenizer.tokenize(input7, Start, List.empty)
  println(s"$input7: $tokens7")
}