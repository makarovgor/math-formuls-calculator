package resultWork.parser.facaiy

import resultWork.parser.facaiy.compiler.MathExpCompiler

sealed trait MathExpError
case class MathExpScannerError(msg: String) extends MathExpError
case class MathExpParserError(msg: String) extends MathExpError

object MathExp {
  def parse(s: String): Expression[String => Double, Double] = {
    val k = s.replace("x","$x").replace("y", "$y").replace("k", "$k")
    var tmp = k
    if(s.contains("Pi")) {
      tmp = s.replace("Pi", math.Pi.toString)
    }
    if (s.contains("E")) {
      tmp = s.replace("E", math.E.toString)
    }
    MathExpCompiler(tmp) match {
      case Right(ts) => Expression.toExpression(ts)
      case Left(e) => throw new IllegalArgumentException(e.toString)
    }
  }
}