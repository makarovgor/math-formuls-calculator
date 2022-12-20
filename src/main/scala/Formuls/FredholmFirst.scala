package Formuls

import Formuls.FredholmSecond.findApprox
import resultWork.parser.facaiy.MathExp

import scala.collection.mutable
import scala.math.BigDecimal.double2bigDecimal

object FredholmFirst {

  def createEmpty(size: Int): Seq[mutable.Seq[Double]] = Seq.fill(size)(mutable.Seq.fill(size)(0))

  def createMatrix(core: String, a: Double, b: Double, n: Int) = {
    val h = (b - a) / n
    val st = (a + h / 2 to b - h / 2 by h).map(_.toDouble)

    val mat = Seq.fill(st.size)(mutable.Seq.fill(st.size)(0.0))
/////
//    val hh = (b - a) / (2 * (n + 1))
//    println(hh)
//
//    val e = (0 to n).map { i =>
//      a + (2 * i + 1) * hh
//    }
////
    mat.indices.foreach{ i =>
      mat.indices.foreach{ j =>
        val fS = MathExp.parse(core).eval(Map("x" -> st(i), "k" -> st(j)))
        mat(i)(j) = 2 * h * fS
      }
    }

    mat.map(_.toSeq)
  }

  def transposedMat(a: Seq[Seq[Double]]) = {
    val res = Seq.fill(a.size)(mutable.Seq.fill(a.size)(0.0))
    a.indices.foreach{ i =>
      a.indices.foreach{ j =>
        res(j)(i) = a(i)(j)
      }
    }
    res.map(_.toSeq)
  }

  def matrixMult(r: Seq[Seq[Double]], q: Seq[Seq[Double]]): Seq[Seq[Double]] = {
    val res = createEmpty(q.size)



    r.indices.foreach { i =>
      r.indices.foreach { j =>
        r.indices.foreach { k =>
          res(i)(j) += r(i)(k) * q(k)(j)
        }
      }
    }
    res.map(_.toSeq)
  }

  def matVecMulti(a: Seq[Seq[Double]], b: Seq[Double]) = {
    val res = mutable.Seq.fill(a.size)(0.0)

    a.indices.foreach { i =>
      a.indices.foreach { j =>
        res(i) += a(i)(j) * b(j)
      }
    }
    res.toSeq
  }

  def vecMatMult(b: Seq[Double], a: Seq[Seq[Double]]) = {
    val res = mutable.Seq.fill(a.size)(0.0)
    a.indices.foreach{ i =>
      a.indices.foreach{ j =>
        res(i) += b(j) * a(j)(i)
      }
    }
    res.toSeq
  }

  def scalMult(a: Seq[Double], b: Seq[Double]) = {
    var res = 0.0
    a.indices.foreach{ i =>
      res += a(i) * b(i)
    }
    res
  }

  def findG(mult: Seq[Double], b: Seq[Double]) = {
    val res = mutable.Seq.fill(b.size)(0.0)
    b.indices.foreach{ i =>
      res(i) = mult(i) - b(i)
    }
    res.toSeq
  }

  def findD(g_prev: Seq[Double], g: Seq[Double], d_prev: Seq[Double]) = {
    val div = scalMult(g, g) / scalMult(g_prev, g_prev)
    val mult = d_prev.map(_ * div)
    val d = mutable.Seq.fill(g.size)(0.0)
    g.indices.foreach{ i =>
      d(i) = mult(i) - g(i)
    }
    d.toSeq
  }

  def findS(d: Seq[Double], g: Seq[Double], a: Seq[Seq[Double]]) = {
    val up = scalMult(d, g)
    val down = scalMult(vecMatMult(d, a), d)
    up / down
  }

  def gradientMethod(a: Seq[Seq[Double]], b: Seq[Double]) = {
    var x_prev = Seq.fill(a.size)(0.0)
    var d_prev = Seq.fill(a.size)(0.0)
    var g_prev = b.map(_ * -1)

    val x = mutable.Seq.fill(a.size)(0.0)
    var g = g_prev
    var d = Seq.fill(a.size)(0.0)

    var it = 0
    while (it < 10) {
      x_prev = x.toSeq

      g_prev = g
      g = findG(matVecMulti(a, x_prev), b)

      d_prev = d
      d = findD(g_prev, g, d_prev)

      val s = findS(d, g, a)

      a.indices.foreach { i =>
        x(i) = x_prev(i) + s * d(i)
      }

      it += 1
    }

    x.toSeq
  }

  def solve(core: String, func: String, min: Double, max: Double, n: Int) = {
    val h = (max - min) / n
    val steps = (min + h / 2 to max - h / 2 by h).map(_.toDouble)

    val a = createMatrix(core, min, max, n)
    val transp = transposedMat(a)
    val b = steps.map(n => MathExp.parse(func).eval(Map("x" -> n)))

    val adjA = matrixMult(transp, a)
    println("--------      " + adjA)
    val adjB = matVecMulti(transp, b)

    val res = gradientMethod(adjA, adjB)

    val f = res.map(n => MathExp.parse(func).eval(Map("x" -> n)))
    val (aFunc, bFunc) = findApprox(res, f)

    (res, aFunc, bFunc)
  }

}
