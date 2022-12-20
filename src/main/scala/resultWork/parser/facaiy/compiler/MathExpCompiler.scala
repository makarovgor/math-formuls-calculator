package resultWork.parser.facaiy.compiler

import resultWork.parser.facaiy.MathExpError

object MathExpCompiler {
  def apply(code: String): Either[MathExpError, MathExpAST] =
    for {
      tokens <- MathExpScanner(code).right
      ast <- MathExpParser(tokens).right
    } yield ast
}
