package Formuls

import resultWork.parser.facaiy.MathExp

import scala.collection.mutable

object Newton {

  def createDF(f: String, x: Double, y: Double, eps: Double) = {
    val dx = (MathExp.parse(f).eval(Map("x" -> (x + eps), "y" -> y)) - MathExp.parse(f).eval(Map("x" -> x, "y" -> y))) / eps
    val dy = (MathExp.parse(f).eval(Map("x" -> x, "y" -> (y + eps))) - MathExp.parse(f).eval(Map("x" -> x, "y" -> y))) / eps
    Seq(dx, dy)
  }

  def createDFDF(f: String, x: Double, y: Double, eps: Double) = {
    def find(a: Double, b: Double) = MathExp.parse(f).eval(Map("x" -> a, "y" -> b))

    val dxdy = (find(x + eps, y + eps) - find(x + eps, y - eps) - find(x - eps, y + eps) + find(x - eps, y - eps)) / (4 * eps * eps)
    val dxdx = find(x + 2 * eps, y) - 2 * find(x, y) + find(x - 2 * eps, y) / (4 * eps * eps)
    val dydy = find(x, y + 2 * eps) - 2 * find(x, y) + find(x, y - 2 * eps) / (4 * eps * eps)
    Seq(Seq(dxdx, dxdy), Seq(dxdy, dydy))
  }

  def reverseDFDF(a: Seq[Seq[Double]]) = {
    val detA = a.head.head * a.last.last - a.head.last * a.last.head
    val minorT = Seq(Seq(a.last.last, a.head.last * -1), Seq(a.last.head * -1, a.head.head))
    minorT.map(_.map(_ * (1 / detA)))
  }

  def multiply(dF: Seq[Double], a: Seq[Seq[Double]]) = {
    Seq(a.head.head * dF.head + a.head.last * dF.last, a.last.head * dF.head + a.last.last * dF.last)
  }

  def solve(f: String, x: Double, y: Double, eps: Double) = {
    var it = 0

    val res = mutable.Seq(x, y)
    var prev = res.toSeq

    do {
      it += 1

      val dF = createDF(f, res.head, res.last, eps)
      val dFdF = createDFDF(f, res.head, res.last, eps)

      val reversedDFDF = reverseDFDF(dFdF)
      val h = multiply(dF, reversedDFDF).map(_ * -1)

      prev = res.toSeq
      res(0) = res.head + h.head
      res(1) = res.last + h.last
    }
    while (it < 10)
    //     while(MathExp.parse(f).eval(Map("x" -> res.head, "y" -> res.last)) < MathExp.parse(f).eval(Map("x" -> prev.head, "y" -> prev.last)))
    (res.head, res.last)
  }
}