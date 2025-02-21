package io.github.quafadas.spireAD

import munit.FunSuite

import spire.*
import spire.math.*
import spire.implicits.*
import _root_.algebra.ring.Field
import spire.algebra.Trig
import spire.algebra.Order
import scala.Numeric
import scala.Fractional
import _root_.algebra.ring.Ring
import spire.compat.fractional
import scala.math.Ordering.Implicits.infixOrderingOps

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

  test("Softmax") {
    val dim = 4
    given jetd: JetDim = JetDim(dim)

    val arg = new JetInstances {}
    given argO: Order[Double] = arg.JetAlgebra[Double].o

    def softmax[T: Trig: ClassTag: Ordering](x: Array[T])(using
        f: Field[T]
    ): Array[T] =
      val xmax = x.fold(f.zero)((a, b) => if a > b then a else b)
      val stabilise = x.map(x => x - xmax)
      val exps = stabilise.map(exp)
      val sumExps = exps.foldLeft(f.zero)(_ + _)
      exps.map(t => t / sumExps)
    end softmax

    def logSoftmax[T: Trig: ClassTag: Ordering](x: Array[T])(using f: Field[T]): Array[T] =
      val xmax = x.fold(f.zero)((a, b) => if a > b then a else b)
      val shifted = x.map(x => x - xmax) // Stabilization
      val logSumExp = log(shifted.map(exp).foldLeft(f.zero)(_ + _))
      shifted.map(x => x - logSumExp)
    end logSoftmax

    def sumSin[T: Trig: ClassTag](x: Array[T])(using
        f: Field[T]
    ) =
      x.map(sin).foldLeft(f.zero)(_ + _)

    val expectedOutput = softmax((1 to dim).toArray.map(_.toDouble))
    val sum = expectedOutput.foldLeft(0.0)(_ + _)
    // assertEquals(sum, 1.0, 1e-6)

    given jd: TejDim[Double] = TejDim()
    // given jNF: JetIsNumericAndFractional[Jet[Double], Double] = jetIsNumericAndFractional[Jet[Double], Double]
    // given tNF: TejIsNumericAndFractional[Tej[Double], Double] = tejIsNumericAndFractional[Jet[Double], Double]

    val range = (1 to dim).toArray.map(_.toDouble).tejArr
    val rangeJ = (1 to dim).toArray.map(_.toDouble).jetArr

    // println(range.mkString(","))
    given oJet: Ordering[Jet[Double]] = Ordering.by(_.real)
    given oTTej: Ordering[Tej[Double]] = Ordering.by(_.tejNum)

    val softmaxResult = logSoftmax(range).foldLeft(Tej(0.0))(_ + _)
    val softmaxResultJ = logSoftmax(rangeJ).foldLeft(Jet(0.0))(_ + _)

    val logSoftmaxResult = logSoftmax(range).foldLeft(Tej(0.0))(_ + _)
    val logSoftmaxResultJ = logSoftmax(rangeJ).foldLeft(Jet(0.0))(_ + _)

    val graph = jd.dag.toGraphviz
    // os.write.over(os.Path("/Users/simon/Code/spire_AD/spireAD/experiments/tmp.dot"), graph)

    val out = softmaxResult.backward(range)
    // println(jd.dag.getAllNodes.mkString("\n"))
    assertEqualsDouble(softmaxResultJ.infinitesimal.sum, 0, 0.0001)
    assertEqualsDouble(out.map(_._2).sum, 0, 0.0001)

  }
end BackwardSuite
