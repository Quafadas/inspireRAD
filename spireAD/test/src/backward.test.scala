package io.github.quafadas.spireAD

import munit.FunSuite

import spire.*
import spire.math.*
import spire.implicits.*
import _root_.algebra.ring.Field
import spire.algebra.Trig
import scala.math.Ordering.Implicits.infixOrderingOps
import narr.*

class BackwardSuite extends FunSuite:

  // def jetIsNumericAndFractional[T: Field: Ring: ClassTag](using jd: JetDim) =
  //   new JetIsNumericAndFractional[Jet[T], T]:
  //     def plus(x: Jet[T], y: Jet[T]): Jet[T] = x + y
  //     def minus(x: Jet[T], y: Jet[T]): Jet[T] = x - y
  //     def times(x: Jet[T], y: Jet[T]): Jet[T] = x * y
  //     def negate(x: Jet[T]): Jet[T] = -x
  //     def fromInt(x: Int): Jet[T] = Jet(summon[Field[T]].fromInt(x))
  //     def toInt(x: Jet[T]): Int = x.toInt
  //     def toLong(x: Jet[T]): Long = x.toLong
  //     def toFloat(x: Jet[T]): Float = x.toFloat
  //     def toDouble(x: Jet[T]): Double = x.toDouble
  //     def compare(x: Jet[T], y: Jet[T]): Int = ??? // if x.real > y.real then 1 else if x.real < y.real then -1 else 0
  //     def div(x: Jet[T], y: Jet[T]): Jet[T] = x / y

  // trait JetIsNumericAndFractional[Jet, T] extends scala.Fractional[T] {}

  // trait TejIsNumericAndFractional[Tej, T] extends scala.Fractional[T] {}

  // def tejIsNumericAndFractional[T: Field: Ring: ClassTag](using td: TejDim[T]) =
  //   new TejIsNumericAndFractional[Tej[T], T]:
  //     def plus(x: Tej[T], y: Tej[T]): Tej[T] = x + y
  //     def minus(x: Tej[T], y: Tej[T]): Tej[T] = x - y
  //     def times(x: Tej[T], y: Tej[T]): Tej[T] = x * y
  //     def negate(x: Tej[T]): Tej[T] = -x
  //     def fromInt(x: Int): Tej[T] = Tej(summon[Field[T]].fromInt(x))
  //     def toInt(x: Tej[T]): Int = x.toInt
  //     def toLong(x: Tej[T]): Long = x.toLong
  //     def toFloat(x: Tej[T]): Float = x.toFloat
  //     def toDouble(x: Tej[T]): Double = x.toDouble
  //     def compare(x: Tej[T], y: Tej[T]): Int = ??? //  if x > y then 1 else if x < y then -1 else 0
  //     def div(x: Tej[T], y: Tej[T]): Tej[T] = x / y
  lazy given oJet: Ordering[Jet[Double]] = Ordering.by(_.real)
  lazy given oTTej: Ordering[Tej[Double]] = Ordering.by(_.value)

  // def writeGraph(using td: TejDim[Double]) =
  //   val graph = td.dag.toGraphviz
  //   os.write.over(os.Path("/Users/simon/Code/spire_AD/spireAD/experiments/tmp.dot"), graph)
  // end writeGraph

  test("log softmax gradients sum to zero. Result are consistent between Tej, Jet, double") {
    val dim = 4
    given jetd: JetDim = JetDim(dim)

    def logSoftmax[T: Trig: ClassTag: Ordering](x: Array[T])(using f: Field[T]): Array[T] =
      val xmax = x.fold(f.zero)((a, b) => if a > b then a else b)
      val shifted = x.map(x => x - xmax) // Stabilization
      val logSumExp = log(shifted.map(exp).foldLeft(f.zero)(_ + _))
      shifted.map(x => x - logSumExp)
    end logSoftmax

    given jd: TejDim[Double] = TejDim()

    val range = (1 to dim).toArray.map(_.toDouble).tejArr
    val rangeJ = (1 to dim).toArray.map(_.toDouble).jetArr

    val res = logSoftmax(range.toArray.map(_.toDouble)).sum
    val logSoftmaxResult = logSoftmax(range).foldLeft(Tej(0.0))(_ + _)
    val logSoftmaxResultJ = logSoftmax(rangeJ).foldLeft(Jet(0.0))(_ + _)

    val out = logSoftmaxResult.backward(range)
    assertEqualsDouble(logSoftmaxResultJ.infinitesimal.sum, 0, 0.0001)
    assertEqualsDouble(out.map(_._2).sum, 0, 0.0001)

    assertEqualsDouble(res, logSoftmaxResultJ.real, 0.0001)
    assertEqualsDouble(res, logSoftmaxResult.value, 0.0001)
  }

  test("softmax gradients sum to zero. Result are consistent between Tej, Jet, double") {
    val dim = 4
    given jetd: JetDim = JetDim(dim)
    given jd: TejDim[Double] = TejDim()

    def softmax[T: Trig: ClassTag](x: Array[T])(using f: Field[T]): Array[T] =
      val expValues = x.map(exp)
      val sumExpValues = expValues.foldLeft(f.zero)(_ + _)
      expValues.map(_ / sumExpValues)
    end softmax

    val range = (1 to dim).toArray.map(_.toDouble).tejArr
    val rangeJ = (1 to dim).toArray.map(_.toDouble).jetArr

    val res = softmax(range.toArray.map(_.toDouble)).sum
    val logSoftmaxResult = softmax(range).foldLeft(Tej(0.0))(_ + _)
    val logSoftmaxResultJ = softmax(rangeJ).foldLeft(Jet(0.0))(_ + _)

    val out = logSoftmaxResult.backward(range)

    assertEqualsDouble(res, 1.0, 0.000001)
    assertEqualsDouble(res, logSoftmaxResultJ.real, 0.0001)
    assertEqualsDouble(res, logSoftmaxResult.value, 0.0001)

  }

end BackwardSuite
