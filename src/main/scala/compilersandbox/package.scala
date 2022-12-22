import compilersandbox.analyser.Analyser
import compilersandbox.compute.Compute
import compilersandbox.parser.{Node, Parser}
import compilersandbox.tokenizer.{Preprocessor, Tokenizer}
import compilersandbox.util.Failure

package object compilersandbox {
  extension (input: String) {
    def evaluate: Either[List[Failure], Either[Long, Double]] = {
      Tokenizer.tokenize(input)
        .map(Preprocessor.preprocess)
        .flatMap(Parser.parse)
        .flatMap(Analyser.analyse)
        .flatMap(Compute.compute)
    }
  }
}
