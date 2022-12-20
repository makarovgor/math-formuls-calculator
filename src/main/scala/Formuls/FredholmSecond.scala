package Formuls

import resultWork.parser.facaiy.MathExp

import scala.collection.mutable
import scala.math.BigDecimal.double2bigDecimal
import scala.math.abs

object FredholmSecond {

  def createMatrix(core: String, a: Double, b: Double, n: Int) = {
    val st = (a to b by ((b - a) / n)).map(_.toDouble)
    val h = (b - a) / n

    val mat = Seq.fill(st.size)(mutable.Seq.fill(st.size)(0.0))
    mat.indices.foreach{ i =>
      mat.indices.foreach{ j =>
        val fS = MathExp.parse(core).eval(Map("x" -> st(i), "k" -> st(j)))
        if(j % 2.0 == 1.0) {
          mat(i)(j) = h * 4 * fS / -3.0
        }
        if(j % 2.0 == 0.0) {
          mat(i)(j) = h * 2 * fS / -3.0
        }
      }
    }

    mat.indices.foreach { i =>
      mat.indices.foreach { j =>
        val fS = MathExp.parse(core).eval(Map("x" -> st(i), "k" -> st(j)))
        if (j == 0 || j == st.size - 1) {
          mat(i)(j) = h * fS / -3.0
        }
        if (i == j) {
          mat(i)(j) = 1.0
        }
      }
    }
    mat
  }

  //method Guassian SLAE
  def solveSLAE(a: Seq[mutable.Seq[Double]], B: Seq[Double]) = {
    val b = mutable.Seq.fill(a.size)(0.0)
    B.indices.foreach{ i =>
      b(i) = B(i)
    }

    val idx = mutable.Seq.fill(a.size)(0)
    a.indices.foreach{ i =>
      idx(i) = i
    }

    a.indices.foreach{ m =>
      var max = abs(a(m)(m))
      var count = m
      a.indices.foreach{ i =>
        if(abs(a(m)(i)) > max) {
          max = abs(a(m)(i))
          count = i
        }
      }

      val temp = idx(m)
      idx(m) = idx(count)
      idx(count) = temp

      (m until a.size).foreach{ i =>
        val temp2 = a(i)(m)
        a(i)(m) = a(i)(count)
        a(i)(count) = temp2
      }

      (m + 1 until a.size).foreach { i =>
        b(i) = b(i) - a(i)(m) * b(m) / a(m)(m)
        (m + 1 until a.size - 1).foreach { j =>
          a(i)(j) = a(i)(j) - a(i)(m) * a(m)(j) / a(m)(m)
        }
      }
    }

    val res = mutable.Seq.fill(a.size)(0.0)
    a.indices.reverse.foreach { i =>
      var sum = 0.0
      (i + 1 until a.size).foreach { j =>
        sum = sum + a(i)(j) * res(idx(j))
      }
      res(idx(i)) = (b(i) - sum) / a(i)(i)
    }
//    Count precision
//    val avrg = BigDecimal(rint(res.sum / res.size)).setScale(1, BigDecimal.RoundingMode.HALF_UP).toDouble
//    val prec = BigDecimal(abs(res.map(_ - avrg).min)).setScale(5, BigDecimal.RoundingMode.HALF_UP).toDouble
    res.toSeq
  }

  def findApprox(x: Seq[Double], y: Seq[Double]) = {
    val sumX = x.sum
    val sumY = y.sum
    val multSum = x.indices.map(i => x(i) * y(i)).sum
    val powSum = x.map(n => n * n).sum

    val deter = sumX * sumX - x.size * powSum
    if(deter != 0.0) {
      val a = (sumX * sumY - x.size * multSum) / deter
      val b = (sumX * multSum - powSum * sumY) / deter
      (a, b)
    }
    else {
      (1.0, 1.0)
    }
  }

  def solve(core: String, func: String, min: Double, max: Double, n: Int) = {
    val steps = (min to max by ((max - min) / n)).map(_.toDouble)
    val a = createMatrix(core, min , max, n)
    val b = steps.map(n => MathExp.parse(func).eval(Map("x" -> n)))
    val res = solveSLAE(a, b)

    val f = res.map(n => MathExp.parse(func).eval(Map("x" -> n)))
    val (aFunc, bFunc) = findApprox(res, f)

    (res, aFunc, bFunc)
  }

}
