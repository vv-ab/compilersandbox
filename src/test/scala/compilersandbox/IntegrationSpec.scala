package compilersandbox

import compilersandbox.analyser.Analyser
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
    val result = Tokenizer.tokenize(input)
      .map(Preprocessor.preprocess)
      .flatMap(Parser.parse)
      .flatMap(Analyser.analyse)
      .flatMap(Compute.compute)

    inside(result) { case Right(Left(value)) =>
      assert(value === 8)
    }
  }

  "Should compute 2(88/22)-2^3" in {

    val input = "2(88/22)-2^3"
    val result = Tokenizer.tokenize(input)
      .map(Preprocessor.preprocess)
      .flatMap(Parser.parse)
      .flatMap(Analyser.analyse)
      .flatMap(Compute.compute)

    inside(result) { case Right(Right(value)) =>
      assert(value === 0.0)
    }
  }

  "Should compute -4*cos(0)(7.5--5)" in {

    val input = "-4*cos(0)(7.5--5)"
    val result = Tokenizer.tokenize(input)
      .map(Preprocessor.preprocess)
      .flatMap(Parser.parse)
      .flatMap(Analyser.analyse)
      .flatMap(Compute.compute)

    inside(result) { case Right(Right(value)) =>
      assert(value === -50.0)
    }
  }

  "should compute sin(30)" in {

    val input = "sin(30)"
    val result = Tokenizer.tokenize(input)
      .map(Preprocessor.preprocess)
      .flatMap(Parser.parse)
      .flatMap(Analyser.analyse)
      .flatMap(Compute.compute)

    inside(result) { case Right(Right(value)) =>
      assert(value === 0.50)
    }
  }

  "should compute SIN(30)" in {

    val input = "SIN(30)"
    val result = Tokenizer.tokenize(input)
      .map(Preprocessor.preprocess)
      .flatMap(Parser.parse)
      .flatMap(Analyser.analyse)
      .flatMap(Compute.compute)

    inside(result) { case Right(Right(value)) =>
      assert(value === 0.50)
    }
  }

  "should fail on 9++" in {

    val input = "9++"
    val result = Tokenizer.tokenize(input)
      .map(Preprocessor.preprocess)
      .flatMap(Parser.parse)
      .flatMap(Analyser.analyse)
      .flatMap(Compute.compute)

    inside(result) { case Left(failures) =>
      assert(failures.size == 1)
    }
  }

  "should compute expressions with constants" in {

    val input = "3pi"
    val result = Tokenizer.tokenize(input)
      .map(Preprocessor.preprocess)
      .flatMap(Parser.parse)
      .flatMap(Analyser.analyse)
      .flatMap(Compute.compute)

    inside(result) { case Right(Right(value)) =>
      assert(value === 9.42477796077)
    }
  }

  "should compute 3!+33" in {

    val input = "3!+33"
    val result = Tokenizer.tokenize(input)
      .map(Preprocessor.preprocess)
      .flatMap(Parser.parse)
      .flatMap(Analyser.analyse)
      .flatMap(Compute.compute)

    inside(result) { case Right(Left(value)) =>
      assert(value === 39)
    }
  }

  "should compute 3!/2" in {

    val input = "3!/2"
    val result = Tokenizer.tokenize(input)
      .map(Preprocessor.preprocess)
      .flatMap(Parser.parse)
      .flatMap(Analyser.analyse)
      .flatMap(Compute.compute)

    inside(result) { case Right(Right(value)) =>
      assert(value === 3.0)
    }
  }

  "should compute sqrt(4)" in {

    val input = "sqrt(4)"
    val result = Tokenizer.tokenize(input)
      .map(Preprocessor.preprocess)
      .flatMap(Parser.parse)
      .flatMap(Analyser.analyse)
      .flatMap(Compute.compute)

    inside(result) { case Right(Right(value)) =>
      assert(value === 2.0)
    }
  }

  "should compute sqrt(36)/2" in {

    val input = "sqrt(36)/2"
    val result = Tokenizer.tokenize(input)
      .map(Preprocessor.preprocess)
      .flatMap(Parser.parse)
      .flatMap(Analyser.analyse)
      .flatMap(Compute.compute)

    inside(result) { case Right(Right(value)) =>
      assert(value === 3.0)
    }
  }

  "should fail on 5.1!" in {

    val input = "5.1!"
    val result = Tokenizer.tokenize(input)
      .map(Preprocessor.preprocess)
      .flatMap(Parser.parse)
      .flatMap(Analyser.analyse)
      .flatMap(Compute.compute)

    inside(result) { case Left(failures) =>
      assert(failures.size == 1)
    }
  }
}
