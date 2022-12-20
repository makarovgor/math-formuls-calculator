package Formuls

import resultWork.parser.facaiy.MathExp

import scala.collection.mutable
import scala.math.abs


object CalcSquare {

  def solve(fst: String, snd: String, thd: String, eps: Double = 0.001) = {
    val f1f2 = root(fst, snd, eps1 = eps)
    val f1f3 = root(fst, thd, eps1 = eps)
    val f2f3 = root(snd, thd, eps1 = eps)

    val f1I = integral(fst, f1f3, f1f2, eps)
    val f2I = integral(snd, f2f3, f1f2, eps)
    val f3I = integral(thd, f1f3, f2f3, eps)

    val squares = Seq(f1I, f2I, f3I).sorted
    val resSq = squares.last - squares.init.sum

    val roots = Seq(f1f2, f1f3, f2f3).sorted

    (resSq, roots.head, roots.last)
  }

  def integral(f: String, a: Double, b: Double, eps2: Double = 0.001): Double = {
    def nSteps(n: Int) = {
      val h = (b - a) / n
      def res(i: Double) = MathExp.parse(f).eval(Map("x" -> i))
      mutable.Seq(res(a), res(b))
        .appendedAll(for (i <- 1 until n by 2) yield 4 * res(a + i * h))
        .appendedAll(for (i <- 2 until n - 1 by 2) yield 2 * res(a + i * h))
        .sum * h / 3
    }

    lazy val I =  (1 to 32).map(nSteps)

    def runge(fst: Int, snd: Int): Double = {
      if (abs(I(fst) - I(snd)) / 15 < eps2)
        I(snd)
      else
        runge(snd, snd * 2 + 1)
    }

    runge(0, 1)
  }

  def root(f: String, g: String, a: Double = -100, b: Double = 100, eps1: Double = 0.001): Double = {
    val c = (a + b) / 2
    def fg(h: Double) = {
      val mp = Map("x" -> h)
      MathExp.parse(f).eval(mp) - MathExp.parse(g).eval(mp)
    }
    if (abs(fg(c)) < eps1) {
      c
    } else if(fg(c) * fg(a) < 0) {
      root(f, g, a, c)
    }
    else {
      root(f, g, c , b)
    }
  }

}
