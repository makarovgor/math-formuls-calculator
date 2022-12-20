package Formuls

import resultWork.parser.facaiy.MathExp

import scala.math.pow

object GoldenRatio {

  def find(f: String, minBound: Double, maxBound : Double, eps: Double) = {

    val phi = (1 + pow(5.0, 0.2)) / 2
    var a = minBound
    var b = maxBound
    var x1 = b - (b - a) / phi
    var x2 = a + (b - a) / phi
    var it = 0

    while((b - a) / 2.0 > eps) {
      val mpX1 = Map("x" -> x1)
      val mpX2 = Map("x" -> x2)
      it += 1
      if (MathExp.parse(f).eval(mpX1) > MathExp.parse(f).eval(mpX2)) {
        a = x1
        x1 = x2
        x2 = b - (x1 - a)
      } else {
        b = x2
        x2 = x1
        x1 = a + (b - x2)
      }

      if (x2 < x1) {
        val temp = x1
        x1 = x2
        x2 = temp
      }
    }
    val resX = (a + b) / 2.0
    (resX, MathExp.parse(f).eval(Map("x" -> resX)))
  }

}
