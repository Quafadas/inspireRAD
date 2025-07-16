import spire._
import spire.math._
import spire.implicits._
import spire.algebra.Trig
import _root_.algebra.ring.Field

import vecxt.arrays.*
import scala.Array.ArrayFactory
import vecxt.RowCol
import vecxt.all.*
import vecxt.BoundsCheck.*
import vecxt.BoundsCheck.DoBoundsCheck.yes
import vecxt.dimMatCheck
import spire.algebra.VectorSpace
import cats.kernel.Order
import spire.syntax.signed
import _root_.algebra.ring.Signed
import _root_.algebra.ring.AdditiveCommutativeMonoid
import spire.algebra.NRoot
import cats.kernel.Eq

object JetArrayTypeClasses:

  given arrEQ: Eq[Array[Double]] = new Eq[Array[Double]] {

    override def eqv(x: Array[Double], y: Array[Double]): Boolean =
      x.sameElements(y)

  }

  given arrjAD(using
      rc: JetDim,
      f: Field[Double]
  ): VectorSpace[Array[Double], Double] =
    new VectorSpace[Array[Double], Double] {

      override inline def negate(x: Array[Double]): Array[Double] =
        vecxt.arrays.*(x)(-1)

      override inline def zero: Array[Double] =
        Array.fill[Double](rc.dimension)(0.0)

      override inline def plus(
          x: Array[Double],
          y: Array[Double]
      ): Array[Double] =
        vecxt.arrays.+(x)(y)

      override inline def timesl(r: Double, v: Array[Double]): Array[Double] =
        vecxt.arrays.*(v)(r)

      override implicit inline def scalar: Field[Double] = f

    }

  given arrAD(using
      rc: JetADDim,
      f: Field[Double]
  ): VectorSpace[Array[Double], Double] =
    new VectorSpace[Array[Double], Double] {

      override inline def negate(x: Array[Double]): Array[Double] = x * -1

      override inline def zero: Array[Double] = Array.fill[Double](rc.size)(0.0)

      override inline def plus(
          x: Array[Double],
          y: Array[Double]
      ): Array[Double] =
        vecxt.arrays.+(x)(y)

      override inline def timesl(r: Double, v: Array[Double]): Array[Double] =
        vecxt.arrays.*(v)(r)

      override implicit inline def scalar: Field[Double] = f

    }

  given matAD(using
      rc: JetADDim,
      fd: Field[Double]
  ): VectorSpace[Matrix[Double], Double] =
    new VectorSpace[Matrix[Double], Double] {

      override inline def negate(x: Matrix[Double]): Matrix[Double] = x * -1

      override inline def zero: Matrix[Double] = Matrix.zeros(rc.rc)

      override inline def plus(
          x: Matrix[Double],
          y: Matrix[Double]
      ): Matrix[Double] =
        x + y

      override inline def timesl(r: Double, v: Matrix[Double]): Matrix[Double] =
        vecxt.DoubleMatrix.*(v)(r)

      override implicit inline def scalar: Field[Double] = fd

    }

  given nrootAD: NRoot[Array[Double]] = new NRoot[Array[Double]] {

    override inline def nroot(a: Array[Double], n: Int): Array[Double] =
      a.clone().map(x => Math.pow(x, 1.0 / n))

    override inline def fpow(
        a: Array[Double],
        b: Array[Double]
    ): Array[Double] =
      a.clone().zip(b).map { case (ai, bi) => Math.pow(ai, bi) }

  }

  given orderAD: Order[Array[Double]] =
    new Order[Array[Double]] {

      override inline def compare(x: Array[Double], y: Array[Double]): Int =
        Ordering[Double].compare(vecxt.arrays.norm(x), (vecxt.arrays.norm(y)))

    }

  given signed(using jd: JetDim): Signed[Array[Double]] =
    new Signed[Array[Double]] {

      override def additiveCommutativeMonoid
          : AdditiveCommutativeMonoid[Array[Double]] =
        new AdditiveCommutativeMonoid[Array[Double]] {

          override def zero: Array[Double] =
            Array.fill[Double](jd.dimension)(0.0)

          override def plus(x: Array[Double], y: Array[Double]): Array[Double] =
            vecxt.arrays.+(x)(y)
        }

      override inline def order: Order[Array[Double]] = orderAD

      override inline def signum(a: Array[Double]): Int =
        if (a.isEmpty) 0
        else {
          val normValue = vecxt.arrays.norm(a)
          if (normValue > 0) 1 else 0
        }

      override def abs(a: Array[Double]): Array[Double] =
        a.clone().map(Math.abs)

    }

  given fadAD(using jd: JetADDim): Field[Array[Double]] =
    new Field[Array[Double]] {
      inline override def negate(x: Array[Double]): Array[Double] =
        vecxt.arrays.*(x)(-1)

      override def zero: Array[Double] = Array.fill[Double](jd.size)(0.0)
      override inline def plus(
          x: Array[Double],
          y: Array[Double]
      ): Array[Double] =
        vecxt.arrays.+(x)(y)

      override inline def div(
          x: Array[Double],
          y: Array[Double]
      ): Array[Double] =
        vecxt.arrays./(x)(y)

      override inline def one: Array[Double] =
        Array.fill[Double](jd.size)(1.0)

      override inline def times(
          x: Array[Double],
          y: Array[Double]
      ): Array[Double] =
        vecxt.arrays.*(x)(y)

    }

  given trigAD(using jd: JetADDim): Trig[Array[Double]] =
    new Trig[Array[Double]] {

      override def e: Array[Double] = Array.fill[Double](jd.rc._2)(Math.E)

      override def pi: Array[Double] = Array.fill[Double](jd.rc._2)(Math.PI)

      override inline def exp(a: Array[Double]): Array[Double] =
        a.exp

      override inline def expm1(a: Array[Double]): Array[Double] =
        a.clone().map(Math.expm1)

      override inline def log(a: Array[Double]): Array[Double] =
        a.log

      override inline def log1p(a: Array[Double]): Array[Double] =
        a.clone().map(Math.log1p)

      override inline def sin(a: Array[Double]): Array[Double] =
        a.clone().map(Math.sin)

      override inline def cos(a: Array[Double]): Array[Double] =
        a.clone().map(Math.cos)

      override inline def tan(a: Array[Double]): Array[Double] =
        a.clone().map(Math.tan)

      override inline def asin(a: Array[Double]): Array[Double] =
        a.clone().map(Math.asin)

      override inline def acos(a: Array[Double]): Array[Double] =
        a.clone().map(Math.acos)

      override inline def atan(a: Array[Double]): Array[Double] =
        a.clone().map(Math.atan)

      override inline def atan2(
          y: Array[Double],
          x: Array[Double]
      ): Array[Double] =
        y.clone().zip(x).map { case (yi, xi) => Math.atan2(yi, xi) }

      override inline def sinh(x: Array[Double]): Array[Double] =
        x.clone().map(Math.sinh)

      override inline def cosh(x: Array[Double]): Array[Double] =
        x.clone().map(Math.cosh)

      override inline def tanh(x: Array[Double]): Array[Double] =
        x.clone().map(Math.tanh)

      override inline def toRadians(a: Array[Double]): Array[Double] =
        a.clone().map(Math.toRadians)

      override inline def toDegrees(a: Array[Double]): Array[Double] =
        a.clone().map(Math.toDegrees)

    }
