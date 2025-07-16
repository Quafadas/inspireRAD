import spire._
import spire.math._
import spire.implicits._
import spire.algebra.Trig
import _root_.algebra.ring.Field

import vecxt.arrays.*
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
import spire.std.SeqInnerProductSpace

object JetMatrixTypeClasses2:

  case class JetMatDim(rc: RowCol) {
    require(rc._1 > 0 && rc._2 > 0)
    def size = rc._1 * rc._2

  }

  // given seqIPSMat: SeqInnerProductSpace[Jet[Matrix[Double]], F[_]] = ???

  given arrEQ: Eq[Matrix[Double]] = new Eq[Matrix[Double]] {

    override def eqv(x: Matrix[Double], y: Matrix[Double]): Boolean =
      if x.shape != y.shape then false
      else x.raw.sameElements(y.raw)

  }

  given arrjAD(using
      rc: JetMatDim,
      f: Field[Double]
  ): VectorSpace[Matrix[Double], Double] =
    new VectorSpace[Matrix[Double], Double] {

      override inline def negate(x: Matrix[Double]): Matrix[Double] =
        Matrix(-x.raw, x.shape)

      override inline def zero: Matrix[Double] = Matrix.zeros(rc.rc)

      override inline def plus(
          x: Matrix[Double],
          y: Matrix[Double]
      ): Matrix[Double] =
        x + y

      override inline def timesl(r: Double, v: Matrix[Double]): Matrix[Double] =
        vecxt.DoubleMatrix.*(v)(r)

      override implicit inline def scalar: Field[Double] = ??? // f

    }

  given arrAD(using
      rc: JetMatDim,
      f: Field[Double]
  ): VectorSpace[Matrix[Double], Double] =
    new VectorSpace[Matrix[Double], Double] {

      override inline def negate(x: Matrix[Double]): Matrix[Double] = x * -1

      override inline def zero: Matrix[Double] = Matrix.zeros(rc.rc)

      override inline def plus(
          x: Matrix[Double],
          y: Matrix[Double]
      ): Matrix[Double] =
        vecxt.DoubleMatrix.+(x)(y)

      override inline def timesl(r: Double, v: Matrix[Double]): Matrix[Double] =
        vecxt.DoubleMatrix.*(v)(r)

      override implicit inline def scalar: Field[Double] = ??? // f

    }

  given matAD(using
      rc: JetMatDim,
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

  given nrootAD: NRoot[Matrix[Double]] = new NRoot[Matrix[Double]] {

    override inline def nroot(a: Matrix[Double], n: Int): Matrix[Double] =
      Matrix(
        a.raw.clone().map(x => Math.pow(x, 1.0 / n)),
        a.shape
      )

    override inline def fpow(
        a: Matrix[Double],
        b: Matrix[Double]
    ): Matrix[Double] =
      Matrix(
        a.raw.clone().zip(b.raw).map { case (ai, bi) => Math.pow(ai, bi) },
        a.shape
      )

  }

  given orderAD: Order[Matrix[Double]] =
    new Order[Matrix[Double]] {

      override inline def compare(x: Matrix[Double], y: Matrix[Double]): Int =
        Ordering[Double].compare(
          vecxt.arrays.norm(x.raw),
          (vecxt.arrays.norm(y.raw))
        )

    }

  given signed(using jd: JetMatDim): Signed[Matrix[Double]] =
    new Signed[Matrix[Double]] {

      override def additiveCommutativeMonoid
          : AdditiveCommutativeMonoid[Matrix[Double]] =
        new AdditiveCommutativeMonoid[Matrix[Double]] {

          override def zero: Matrix[Double] = Matrix.zeros(jd.rc)

          override def plus(
              x: Matrix[Double],
              y: Matrix[Double]
          ): Matrix[Double] =
            x + y
        }

      override inline def order: Order[Matrix[Double]] = orderAD

      override inline def signum(a: Matrix[Double]): Int =
        if (a.shape == (0, 0)) 0
        else {
          a.raw.find(_ != 0.0).map(_ => 1).getOrElse(0)
        }

      override def abs(a: Matrix[Double]): Matrix[Double] =
        Matrix(a.raw.clone().map(Math.abs), a.shape)

    }

  given fadAD(using jd: JetMatDim): Field[Matrix[Double]] =
    new Field[Matrix[Double]] {
      inline override def negate(x: Matrix[Double]): Matrix[Double] =
        x * -1

      override def zero: Matrix[Double] = Matrix.zeros(jd.rc)

      override inline def plus(
          x: Matrix[Double],
          y: Matrix[Double]
      ): Matrix[Double] =
        x + y

      override inline def div(
          x: Matrix[Double],
          y: Matrix[Double]
      ): Matrix[Double] =
        x / y

      override inline def one: Matrix[Double] =
        Matrix.eye[Double](
          jd.rc._1
        ) // Otherwise scala multiplication doesn't work

      override inline def times(
          x: Matrix[Double],
          y: Matrix[Double]
      ): Matrix[Double] =
        x * y

    }

  given trigAD(using jd: JetMatDim): Trig[Matrix[Double]] =
    new Trig[Matrix[Double]] {

      override def e: Matrix[Double] =
        Matrix.fill[Double](Math.E, jd.rc)

      override def pi: Matrix[Double] =
        Matrix.fill[Double](Math.PI, jd.rc)

      override inline def exp(a: Matrix[Double]): Matrix[Double] =
        a.exp

      override inline def expm1(a: Matrix[Double]): Matrix[Double] =
        Matrix(a.raw.map(Math.expm1), a.shape)

      override inline def log(a: Matrix[Double]): Matrix[Double] =
        a.log

      override inline def log1p(a: Matrix[Double]): Matrix[Double] =
        Matrix(a.raw.map(Math.log1p), a.shape)

      override inline def sin(a: Matrix[Double]): Matrix[Double] =
        Matrix(a.raw.map(Math.sin), a.shape)

      override inline def cos(a: Matrix[Double]): Matrix[Double] =
        Matrix(a.raw.map(Math.cos), a.shape)

      override inline def tan(a: Matrix[Double]): Matrix[Double] =
        Matrix(a.raw.map(Math.tan), a.shape)

      override inline def asin(a: Matrix[Double]): Matrix[Double] =
        Matrix(a.raw.map(Math.asin), a.shape)

      override inline def acos(a: Matrix[Double]): Matrix[Double] =
        Matrix(a.raw.map(Math.acos), a.shape)

      override inline def atan(a: Matrix[Double]): Matrix[Double] =
        Matrix(a.raw.map(Math.atan), a.shape)

      override inline def atan2(
          y: Matrix[Double],
          x: Matrix[Double]
      ): Matrix[Double] =
        Matrix(
          y.raw.clone().zip(x.raw).map { case (yi, xi) => Math.atan2(yi, xi) },
          y.shape
        )

      override inline def sinh(x: Matrix[Double]): Matrix[Double] =
        Matrix(x.raw.clone().map(Math.sinh), x.shape)

      override inline def cosh(x: Matrix[Double]): Matrix[Double] =
        Matrix(x.raw.clone().map(Math.cosh), x.shape)

      override inline def tanh(x: Matrix[Double]): Matrix[Double] =
        Matrix(x.raw.clone().map(Math.tanh), x.shape)

      override inline def toRadians(a: Matrix[Double]): Matrix[Double] =
        Matrix(a.raw.map(Math.toRadians), a.shape)

      override inline def toDegrees(a: Matrix[Double]): Matrix[Double] =
        Matrix(a.raw.map(Math.toDegrees), a.shape)

    }
