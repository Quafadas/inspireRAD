package io.github.quafadas.inspireRAD

import scala.specialized as sp
import vecxt.BoundsCheck.DoBoundsCheck.yes
import cats.kernel.Semigroup
import vecxt.all.*
import cats.Id
import scala.annotation.targetName
import scala.reflect.ClassTag
import vecxt.BoundsCheck
import spire.math.Number
import narr.*
import spire.*
import spire.math.Jet
import spire.math.JetDim
import spire.implicits.*
import spire.compat.numeric
import cats.kernel.Order
import _root_.algebra.ring.AdditiveCommutativeMonoid

final case class Scalar[T](scalar: T)
object VectorisedField:

  given jetNumeric(using jd: JetDim): Numeric[Jet[Double]] = new Numeric[Jet[Double]]:

    override def parseString(str: String): Option[Jet[Double]] = ???

    override def plus(x: Jet[Double], y: Jet[Double]): Jet[Double] = x + y
    override def minus(x: Jet[Double], y: Jet[Double]): Jet[Double] = x - y
    override def times(x: Jet[Double], y: Jet[Double]): Jet[Double] = x * y
    override def negate(x: Jet[Double]): Jet[Double] = -x
    override def zero: Jet[Double] = Jet.zero[Double]
    override def abs(x: Jet[Double]): Jet[Double] = x.abs
    override def signum(x: Jet[Double]): Int = ???

    // Comparison operations
    override def compare(x: Jet[Double], y: Jet[Double]): Int = ???

    // Conversion operations
    override def fromInt(n: Int): Jet[Double] = Jet(n.toDouble)

    override def toInt(x: Jet[Double]): Int = ???
    override def toLong(x: Jet[Double]): Long = ???
    override def toFloat(x: Jet[Double]): Float = ???
    override def toDouble(x: Jet[Double]): Double = ???

  given tejNumeric(using jd: TejDim[Double]): Numeric[Tej[Double]] = new Numeric[Tej[Double]]:

    override def parseString(str: String): Option[Tej[Double]] = ???

    override def plus(x: Tej[Double], y: Tej[Double]): Tej[Double] = x + y
    override def minus(x: Tej[Double], y: Tej[Double]): Tej[Double] = x - y
    override def times(x: Tej[Double], y: Tej[Double]): Tej[Double] = x * y
    override def negate(x: Tej[Double]): Tej[Double] = -x
    override def zero: Tej[Double] = Tej.zero[Double]
    override def abs(x: Tej[Double]): Tej[Double] = x.abs
    override def signum(x: Tej[Double]): Int = ???

    // Comparison operations
    override def compare(x: Tej[Double], y: Tej[Double]): Int = ???

    // Conversion operations
    override def fromInt(n: Int): Tej[Double] = Tej(n.toDouble)

    override def toInt(x: Tej[Double]): Int = ???
    override def toLong(x: Tej[Double]): Long = ???
    override def toFloat(x: Tej[Double]): Float = ???
    override def toDouble(x: Tej[Double]): Double = ???

  def scalarJetField(using jd: JetDim): VectorisedField[Scalar, Jet[Double]] = new VectorisedField[Scalar, Jet[Double]]:

    override def sum(x: Scalar[Jet[Double]])(using ClassTag[Jet[Double]]): Jet[Double] = x.scalar
    override def abs(x: Scalar[Jet[Double]]): Scalar[Jet[Double]] = Scalar(x.scalar.abs)
    override def sign(x: Scalar[Jet[Double]])(using ClassTag[Jet[Double]]): Scalar[Jet[Double]] =
      Scalar(Jet(if x.scalar.real > 0.0 then 1.0 else if x.scalar.real < 0.0 then -1.0 else 0.0))

    override val numDimensions = 0
    extension (a: Scalar[Jet[Double]])
      override def >(y: Jet[Double]): Scalar[Boolean] = Scalar(a.scalar.real > y.real)
      override def clampMin(min: Jet[Double]): Scalar[Jet[Double]] = Scalar(Jet(Math.max(a.scalar.real, min.real)))

      override def *:*(y: Scalar[Boolean]): Scalar[Jet[Double]] =
        if y.scalar then a else Scalar(Jet.zero[Double])
    end extension

    extension (a: Double)
      override def const: Jet[Double] = Jet(a)
      override def /(b: Scalar[Jet[Double]]): Scalar[Jet[Double]] = Scalar(Jet(a) / b.scalar)
    end extension

    override def fromDouble(x: Double): Jet[Double] = Jet(x)

    override def zero(x: Scalar[Jet[Double]]): Scalar[Jet[Double]] = Scalar(Jet.zero[Double])

    override def one(x: Scalar[Jet[Double]])(using ClassTag[Jet[Double]]): Scalar[Jet[Double]] = Scalar(Jet.one[Double])

    override def allOnes(x: Scalar[Jet[Double]])(using ClassTag[Jet[Double]]): Scalar[Jet[Double]] = Scalar(
      Jet.one[Double]
    )

    extension (a: Scalar[Jet[Double]])
      override def unary_- : Scalar[Jet[Double]] = Scalar(-a.scalar)
      override def productExceptSelf(): Scalar[Jet[Double]] = Scalar(Jet.one[Double])
      override def numel: Int = 1
      override def +(x: Scalar[Jet[Double]]): Scalar[Jet[Double]] = Scalar(a.scalar + x.scalar)
      @targetName("rhs+")
      override def +(x: Jet[Double]): Scalar[Jet[Double]] = Scalar(a.scalar + x)
      override def -(y: Scalar[Jet[Double]]): Scalar[Jet[Double]] = Scalar(a.scalar - y.scalar)
      @targetName("rhs-")
      override def -(y: Jet[Double]): Scalar[Jet[Double]] = Scalar(a.scalar - y)
      override def *(y: Scalar[Jet[Double]]): Scalar[Jet[Double]] = Scalar(a.scalar * y.scalar)
      @targetName("rhs*")
      override def *(y: Jet[Double]): Scalar[Jet[Double]] = Scalar(a.scalar * y)
      override def /(y: Scalar[Jet[Double]]): Scalar[Jet[Double]] = Scalar(a.scalar / y.scalar)
      @targetName("rhs/")
      override def /(y: Jet[Double]): Scalar[Jet[Double]] = Scalar(a.scalar / y)
    end extension

  def scalarTejField(using jd: TejDim[Double]): VectorisedField[Scalar, Tej[Double]] =
    new VectorisedField[Scalar, Tej[Double]]:

      override def sum(x: Scalar[Tej[Double]])(using ClassTag[Tej[Double]]): Tej[Double] = x.scalar
      override def abs(x: Scalar[Tej[Double]]): Scalar[Tej[Double]] = Scalar(x.scalar.abs)
      override def sign(x: Scalar[Tej[Double]])(using ClassTag[Tej[Double]]): Scalar[Tej[Double]] =
        Scalar(Tej(if x.scalar.value > 0.0 then 1.0 else if x.scalar.value < 0.0 then -1.0 else 0.0))

      override val numDimensions = 0
      extension (a: Double)
        override def const: Tej[Double] = Tej(a)
        override def /(b: Scalar[Tej[Double]]): Scalar[Tej[Double]] = Scalar(Tej(a) / b.scalar)
      end extension

      extension (a: Scalar[Tej[Double]])
        override def >(y: Tej[Double]): Scalar[Boolean] = Scalar(a.scalar.value > y.value)
        override def clampMin(min: Tej[Double]): Scalar[Tej[Double]] = Scalar(Tej(Math.max(a.scalar.value, min.value)))
        override def *:*(y: Scalar[Boolean]): Scalar[Tej[Double]] =
          if y.scalar then a else Scalar(Tej.zero[Double])
        @targetName("rhs-")
        override def -(y: Tej[Double]): Scalar[Tej[Double]] = Scalar(a.scalar - y)
      end extension

      override def fromDouble(x: Double): Tej[Double] = Tej(x)

      override def zero(x: Scalar[Tej[Double]]): Scalar[Tej[Double]] = Scalar(Tej.zero[Double])

      override def one(x: Scalar[Tej[Double]])(using ClassTag[Tej[Double]]): Scalar[Tej[Double]] = Scalar(
        Tej.one[Double]
      )

      override def allOnes(x: Scalar[Tej[Double]])(using ClassTag[Tej[Double]]): Scalar[Tej[Double]] = Scalar(
        Tej.one[Double]
      )

      extension (a: Scalar[Tej[Double]])
        override def unary_- : Scalar[Tej[Double]] = Scalar(-a.scalar)
        override def productExceptSelf(): Scalar[Tej[Double]] = Scalar(Tej.one[Double])
        override def numel: Int = 1
        override def +(x: Scalar[Tej[Double]]): Scalar[Tej[Double]] = Scalar(a.scalar + x.scalar)
        @targetName("rhs+")
        override def +(x: Tej[Double]): Scalar[Tej[Double]] = Scalar(a.scalar + x)
        override def -(y: Scalar[Tej[Double]]): Scalar[Tej[Double]] = Scalar(a.scalar - y.scalar)
        override def *(y: Scalar[Tej[Double]]): Scalar[Tej[Double]] = Scalar(a.scalar * y.scalar)
        @targetName("rhs*")
        override def *(y: Tej[Double]): Scalar[Tej[Double]] = Scalar(a.scalar * y)
        override def /(y: Scalar[Tej[Double]]): Scalar[Tej[Double]] = Scalar(a.scalar / y.scalar)
        @targetName("rhs/")
        override def /(y: Tej[Double]): Scalar[Tej[Double]] = Scalar(a.scalar / y)
      end extension

  def elementwiseMatrixJetField(using jd: JetDim): VectorisedField[Matrix, Jet[Double]] =
    new VectorisedField[Matrix, Jet[Double]]:

      override def sum(x: Matrix[Jet[Double]])(using ClassTag[Jet[Double]]): Jet[Double] =
        x.raw.foldLeft(Jet.zero[Double])(_ + _)
      override def abs(x: Matrix[Jet[Double]]): Matrix[Jet[Double]] = Matrix(x.shape, x.raw.map(_.abs))
      override def sign(x: Matrix[Jet[Double]])(using ClassTag[Jet[Double]]): Matrix[Jet[Double]] =
        Matrix(x.shape, x.raw.map(jet => Jet(if jet.real > 0.0 then 1.0 else if jet.real < 0.0 then -1.0 else 0.0)))

      override val numDimensions = 2
      extension (a: Matrix[Jet[Double]])
        override def >(y: Jet[Double]): Matrix[Boolean] =
          Matrix(a.shape, a.raw.zipWithIndex.map { case (v, i) => v.real > y.real })
        override def clampMin(min: Jet[Double]): Matrix[Jet[Double]] =
          Matrix(a.shape, a.raw.map(tej => Jet(Math.max(tej.real, min.real))))

        override def *:*(y: Matrix[Boolean]): Matrix[Jet[Double]] = ???
      end extension

      def fromDouble(x: Double): Jet[Double] = Jet(x)
      def zero(x: Matrix[Jet[Double]]): Matrix[Jet[Double]] =
        Matrix[Jet[Double]](x.shape, Array.fill(x.raw.length)(0.0).jetArr)

      def one(x: Matrix[Jet[Double]])(using ClassTag[Jet[Double]]) =
        val easyI = Matrix.eye[Double](x.shape)
        Matrix(x.shape, easyI.raw.jetArr)
      end one

      def allOnes(x: Matrix[Jet[Double]])(using ClassTag[Jet[Double]]) =
        val easyI = Matrix.ones[Double](x.shape)
        Matrix(x.shape, easyI.raw.jetArr)
      end allOnes

      extension (a: Double)
        def const: Jet[Double] = Jet(a)
        def /(b: Matrix[Jet[Double]]): Matrix[Jet[Double]] = Matrix[Jet[Double]](
          b.raw.map(_ / a),
          b.shape
        )
      end extension

      extension (a: Matrix[Jet[Double]])
        inline def productExceptSelf(): Matrix[Jet[Double]] = ???
        def numel: Int = a.shape(0) * a.shape(1)
        inline def unary_- : Matrix[Jet[Double]] = Matrix[Jet[Double]](a.raw.map(_ * -1), a.shape)
        inline def +(x: Matrix[Jet[Double]]): Matrix[Jet[Double]] =
          Matrix[Jet[Double]](a.raw.zip(x.raw).map(_ * _), a.shape)
        @targetName("rhs+")
        inline def +(x: Jet[Double]): Matrix[Jet[Double]] =
          Matrix[Jet[Double]](a.raw.map(_ + x), a.shape)
        inline def -(y: Matrix[Jet[Double]]): Matrix[Jet[Double]] =
          Matrix[Jet[Double]](a.raw.zip(y.raw).map(_ - _), a.shape)
        @targetName("rhs-")
        inline def -(x: Jet[Double]): Matrix[Jet[Double]] = Matrix[Jet[Double]](a.raw.map(_ - x), a.shape)
        inline def *(y: Matrix[Jet[Double]]): Matrix[Jet[Double]] =
          Matrix[Jet[Double]](a.raw.zip(y.raw).map(_ * _), a.shape)
        @targetName("rhs*")
        inline def *(y: Jet[Double]): Matrix[Jet[Double]] = Matrix[Jet[Double]](a.raw.map(_ / y), a.shape)
        inline def /(y: Matrix[Jet[Double]]): Matrix[Jet[Double]] =
          Matrix[Jet[Double]](a.raw.zip(y.raw).map(_ / _), a.shape)
        @targetName("rhs/")
        inline def /(y: Jet[Double]): Matrix[Jet[Double]] = Matrix[Jet[Double]](a.raw.map(_ * y), a.shape)
      end extension

  def elementwiseArrayJetField(using jd: JetDim): VectorisedField[NArray, Jet[Double]] =
    new VectorisedField[NArray, Jet[Double]]:

      override def sum(x: Array[Jet[Double]])(using ClassTag[Jet[Double]]): Jet[Double] =
        x.foldLeft(Jet.zero[Double])(_ + _)
      override def abs(x: Array[Jet[Double]]): Array[Jet[Double]] = x.map(_.abs)
      override def sign(x: Array[Jet[Double]])(using ClassTag[Jet[Double]]): Array[Jet[Double]] =
        x.map(jet => Jet(if jet.real > 0.0 then 1.0 else if jet.real < 0.0 then -1.0 else 0.0))

      override val numDimensions = 1

      extension (a: NArray[Jet[Double]])
        override def >(y: Jet[Double]): NArray[Boolean] = a.map(_.real > y.real)
        override def clampMin(min: Jet[Double]): NArray[Jet[Double]] = a.map(tej => Jet(Math.max(tej.real, min.real)))
        override def *:*(y: NArray[Boolean]): NArray[Jet[Double]] =
          a.zip(y).map { case (tej, bool) => if bool then tej else Jet.zero[Double] }
      end extension

      def fromDouble(x: Double): Jet[Double] = Jet(x)
      def zero(x: NArray[Jet[Double]]): NArray[Jet[Double]] = NArray.fill[Jet[Double]](x.length)(Jet.zero[Double])
      def one(x: NArray[Jet[Double]])(using ClassTag[Jet[Double]]): NArray[Jet[Double]] =
        NArray.fill[Jet[Double]](x.length)(Jet.one[Double])
      def allOnes(x: NArray[Jet[Double]])(using ClassTag[Jet[Double]]) = one(x)

      extension (a: Double)
        def const: Jet[Double] = Jet(a)
        def /(b: NArray[Jet[Double]]): NArray[Jet[Double]] =
          val ja = Jet(a)
          b.map((bb: Jet[Double]) => ja / bb)
        end /
      end extension

      extension (a: NArray[Jet[Double]])
        inline def productExceptSelf(): NArray[Jet[Double]] = ???

        def numel: Int = a.length
        inline def /(y: NArray[Jet[Double]]): NArray[Jet[Double]] = a.zip(y).map((ai, bi) => ai / bi)
        @targetName("rhs/")
        inline def /(y: Jet[Double]): NArray[Jet[Double]] = a.map(_ / y)
        inline def unary_- : NArray[Jet[Double]] = a.map(_ - 1.0)
        inline def +(x: NArray[Jet[Double]]): NArray[Jet[Double]] = a.zip(x).map((a, b) => a + b)
        @targetName("rhs+")
        inline def +(x: Jet[Double]): NArray[Jet[Double]] = a.map(_ + x)
        inline def -(y: NArray[Jet[Double]]): NArray[Jet[Double]] = a.zip(y).map((a, b) => a - b)
        @targetName("rhs-")
        inline def -(x: Jet[Double]): NArray[Jet[Double]] = a.map(_ - x)
        inline def *(y: NArray[Jet[Double]]): NArray[Jet[Double]] = a.zip(y).map((a, b) => a * b)
        @targetName("rhs*")
        inline def *(y: Jet[Double]): NArray[Jet[Double]] = a.map(_ * y)
      end extension

  def elementwiseArrayJetField(using jd: TejDim[Double]): VectorisedField[NArray, Tej[Double]] =
    new VectorisedField[NArray, Tej[Double]]:

      override def sum(x: Array[Tej[Double]])(using ClassTag[Tej[Double]]): Tej[Double] =
        x.foldLeft(Tej.zero[Double])(_ + _)
      override def abs(x: Array[Tej[Double]]): Array[Tej[Double]] = x.map(_.abs)
      override def sign(x: Array[Tej[Double]])(using ClassTag[Tej[Double]]): Array[Tej[Double]] =
        x.map(tej => Tej(if tej.value > 0.0 then 1.0 else if tej.value < 0.0 then -1.0 else 0.0))

      override val numDimensions = 1

      extension (a: Array[Tej[Double]])
        override def >(y: Tej[Double]): Array[Boolean] = a.map(_.value > y.value)
        override def clampMin(min: Tej[Double]): NArray[Tej[Double]] = a.map(tej => Tej(Math.max(tej.value, min.value)))
        override def *:*(y: NArray[Boolean]): NArray[Tej[Double]] =
          a.zip(y).map { case (tej, bool) => if bool then tej else Tej.zero[Double] }
      end extension

      def fromDouble(x: Double): Tej[Double] = Tej(x)
      def zero(x: NArray[Tej[Double]]): NArray[Tej[Double]] = NArray.fill[Tej[Double]](x.length)(Tej.zero[Double])
      def one(x: NArray[Tej[Double]])(using ClassTag[Tej[Double]]): NArray[Tej[Double]] =
        NArray.fill[Tej[Double]](x.length)(Tej.one[Double])
      def allOnes(x: NArray[Tej[Double]])(using ClassTag[Tej[Double]]) = one(x)

      extension (a: Double)
        def const: Tej[Double] = Tej(a)
        def /(b: NArray[Tej[Double]]): NArray[Tej[Double]] =
          val constA = Tej(a)
          b.map(constA / _)
        end /
      end extension

      extension (a: NArray[Tej[Double]])
        inline def productExceptSelf(): NArray[Tej[Double]] = ???

        def numel: Int = a.length
        inline def /(y: NArray[Tej[Double]]): NArray[Tej[Double]] = a.zip(y).map((ai, bi) => ai / bi)
        @targetName("rhs/")
        inline def /(y: Tej[Double]): NArray[Tej[Double]] = a.map(_ / y)
        inline def unary_- : NArray[Tej[Double]] = a.map(_ - 1.0)
        inline def +(x: NArray[Tej[Double]]): NArray[Tej[Double]] = a.zip(x).map((a, b) => a + b)
        @targetName("rhs+")
        inline def +(x: Tej[Double]): NArray[Tej[Double]] = a.map(_ + x)
        inline def -(y: NArray[Tej[Double]]): NArray[Tej[Double]] = a.zip(y).map((a, b) => a - b)
        @targetName("rhs-")
        inline def -(x: Tej[Double]): NArray[Tej[Double]] = a.map(_ - x)
        inline def *(y: NArray[Tej[Double]]): NArray[Tej[Double]] = a.zip(y).map((a, b) => a * b)
        @targetName("rhs*")
        inline def *(y: Tej[Double]): NArray[Tej[Double]] = a.map(_ * y)
      end extension

  given scalarField: VectorisedField[Scalar, Double] = new VectorisedField[Scalar, Double]:

    override def sum(x: Scalar[Double])(using ClassTag[Double]): Double = x.scalar
    override def abs(x: Scalar[Double]): Scalar[Double] = Scalar(x.scalar.abs)
    override def sign(x: Scalar[Double])(using ClassTag[Double]): Scalar[Double] =
      Scalar(if x.scalar > 0.0 then 1.0 else if x.scalar < 0.0 then -1.0 else 0.0)

    override val numDimensions = 0

    extension (a: Scalar[Double])
      override def >(y: Double): Scalar[Boolean] = Scalar(a.scalar > y)
      override def clampMin(y: Double): Scalar[Double] = Scalar(a.scalar.max(y))

      override def *:*(y: Scalar[Boolean]): Scalar[Double] =
        if y.scalar then a else Scalar(0.0)
    end extension

    extension (a: Double)
      override def const: Double = a
      override def /(b: Scalar[Double]): Scalar[Double] = Scalar(a / b.scalar)
    end extension

    override def fromDouble(x: Double): Double = x

    override def zero(x: Scalar[Double]): Scalar[Double] = Scalar(0.0)

    override def one(x: Scalar[Double])(using ClassTag[Double]): Scalar[Double] = Scalar(1.0)

    override def allOnes(x: Scalar[Double])(using ClassTag[Double]): Scalar[Double] = Scalar(1.0)

    extension (a: Scalar[Double])

      override def unary_- : Scalar[Double] = Scalar(-a.scalar)

      override def productExceptSelf(): Scalar[Double] = Scalar(1.0)
      override def numel: Int = 1
      override def +(x: Scalar[Double]): Scalar[Double] = Scalar(a.scalar + x.scalar)
      @targetName("rhs+")
      override def +(x: Double): Scalar[Double] = Scalar(a.scalar + x)
      override def -(y: Scalar[Double]): Scalar[Double] = Scalar(a.scalar - y.scalar)
      @targetName("rhs-")
      override def -(y: Double): Scalar[Double] = Scalar(a.scalar - y)
      override def *(y: Scalar[Double]): Scalar[Double] = Scalar(a.scalar * y.scalar)
      @targetName("rhs*")
      override def *(y: Double): Scalar[Double] = Scalar(a.scalar * y)

      override def /(y: Scalar[Double]): Scalar[Double] = Scalar(a.scalar / y.scalar)

      @targetName("rhs/")
      override def /(y: Double): Scalar[Double] = Scalar(a.scalar / y)
    end extension

  given elementwiseMatrixDoubleField: VectorisedField[Matrix, Double] = new VectorisedField[Matrix, Double]:
    override def sum(x: Matrix[Double])(using ClassTag[Double]): Double = vecxt.arrays.sum(x.raw)
    override def abs(x: Matrix[Double]): Matrix[Double] = Matrix(x.shape, x.raw.map(_.abs))
    override def sign(x: Matrix[Double])(using ClassTag[Double]): Matrix[Double] =
      Matrix(x.shape, x.raw.map(d => if d > 0.0 then 1.0 else if d < 0.0 then -1.0 else 0.0))

    override val numDimensions = 2

    extension (a: Matrix[Double])

      override def clampMin(min: Double): Matrix[Double] =
        Matrix[Double](vecxt.arrays.clampMin(a.raw)(min), a.shape)

      override def >(y: Double): Matrix[Boolean] =
        Matrix[Boolean](vecxt.arrays.>(a.raw)(y), a.shape)

      override def *:*(y: Matrix[Boolean]): Matrix[Double] =
        vecxt.all.*:*(a)(y)
    end extension

    def fromDouble(x: Double): Double = x

    def zero(x: Matrix[Double]): Matrix[Double] = Matrix.zeros[Double](x.shape)
    def one(x: Matrix[Double])(using ClassTag[Double]) = Matrix.eye(x.shape(0))

    def allOnes(x: Matrix[Double])(using ClassTag[Double]) = Matrix.ones(x.shape)

    extension (a: Double)
      def const: Double = a
      def /(b: Matrix[Double]): Matrix[Double] = Matrix[Double](vecxt.arrays./(a)(b.raw), b.shape)
    end extension

    extension (a: Matrix[Double])
      inline def productExceptSelf(): Matrix[Double] =
        Matrix[Double](vecxt.arrays.productExceptSelf(a.raw), a.shape)

      def numel: Int = a.shape(0) * a.shape(1)
      inline def unary_- : Matrix[Double] = Matrix[Double](vecxt.arrays.unary_-(a.raw), a.shape)
      inline def +(x: Matrix[Double]): Matrix[Double] = vecxt.all.+(x)(a)

      @targetName("rhs+")
      inline def +(x: Double): Matrix[Double] = vecxt.all.+(a)(x)

      inline def -(y: Matrix[Double]): Matrix[Double] = vecxt.all.-(a)(y)

      @targetName("rhs-")
      inline def -(x: Double): Matrix[Double] = vecxt.all.-(a)(x)

      inline def *(y: Matrix[Double]): Matrix[Double] = vecxt.all.hadamard(a)(y)

      @targetName("rhs*")
      inline def *(y: Double): Matrix[Double] = vecxt.all.*(a)(y)
      inline def /(y: Matrix[Double]): Matrix[Double] = vecxt.all./:/(a)(y)
      @targetName("rhs/")
      inline def /(y: Double): Matrix[Double] = Matrix[Double](vecxt.arrays./(a.raw)(y), a.shape)

    end extension

  given elementwiseArrayDoubleField: VectorisedField[NArray, Double] = new VectorisedField[NArray, Double]:

    override def sum(x: Array[Double])(using ClassTag[Double]): Double = vecxt.arrays.sum(x)
    override def abs(x: Array[Double]): Array[Double] = x.map(_.abs)
    override def sign(x: Array[Double])(using ClassTag[Double]): Array[Double] =
      x.map(d => if d > 0.0 then 1.0 else if d < 0.0 then -1.0 else 0.0)

    override val numDimensions = 1
    extension (a: Array[Double])

      override def >(y: Double): Array[Boolean] = vecxt.arrays.>(a)(y)
      override def clampMin(min: Double): Array[Double] = vecxt.arrays.clampMin(a)(min)
      override def *:*(y: Array[Boolean]): Array[Double] = vecxt.arrays.apply(a)(y)
    end extension

    def fromDouble(x: Double): Double = x
    def zero(x: NArray[Double]): NArray[Double] = NArray.fill[Double](x.length)(0.0)
    def one(x: NArray[Double])(using ClassTag[Double]): NArray[Double] = NArray.fill[Double](x.length)(1.0)

    def allOnes(x: NArray[Double])(using ClassTag[Double]) = one(x)

    extension (a: Double)
      def const: Double = a
      def /(b: NArray[Double]): NArray[Double] = vecxt.arrays./(a)(b)

    end extension

    extension (a: NArray[Double])
      inline def productExceptSelf(): NArray[Double] = vecxt.arrays.productExceptSelf(a)
      def numel: Int = a.length

      inline def /(y: NArray[Double]): NArray[Double] = vecxt.arrays./(a)(y)

      @targetName("rhs/")
      inline def /(y: Double): NArray[Double] = vecxt.arrays./(a)(y)

      inline def unary_- : NArray[Double] = vecxt.arrays.*(a)(-1)
      inline def +(x: NArray[Double]): NArray[Double] = vecxt.arrays.+(x)(a)

      @targetName("rhs+")
      inline def +(x: Double): NArray[Double] = vecxt.arrays.+(a)(x)

      inline def -(y: NArray[Double]): NArray[Double] = vecxt.arrays.-(a)(y)
      @targetName("rhs-")
      inline def -(x: Double): NArray[Double] = vecxt.arrays.-(a)(x)

      inline def *(y: NArray[Double]): NArray[Double] = vecxt.arrays.*(a)(y)

      @targetName("rhs*")
      inline def *(y: Double): NArray[Double] = vecxt.arrays.*(a)(y)

    end extension

end VectorisedField

trait VectorisedField[F[_], @sp(Double) A]:
  val numDimensions: Int
  def sum(x: F[A])(using ClassTag[A]): A
  def fromDouble(x: Double): A
  def zero(x: F[A]): F[A]
  def one(x: F[A])(using ClassTag[A]): F[A]
  def allOnes(x: F[A])(using ClassTag[A]): F[A]
  def abs(x: F[A]): F[A]
  def sign(x: F[A])(using ClassTag[A]): F[A]

  extension (a: Double)
    def const: A
    def /(b: F[A]): F[A]
  end extension

  extension (a: F[A])
    def unary_- : F[A]
    def productExceptSelf(): F[A]
    def numel: Int

    // def +=(x: F[A]): F[A]
    def +(x: F[A]): F[A]
    @targetName("rhs+")
    def +(x: A): F[A]
    def -(y: F[A]): F[A]
    @targetName("rhs-")
    def -(y: A): F[A]

    def *(y: F[A]): F[A]
    @targetName("rhs*")
    def *(y: A): F[A]

    def /(y: F[A]): F[A]
    @targetName("rhs/")
    def /(y: A): F[A]

    def >(y: A): F[Boolean]
    def clampMin(min: A): F[A]
    def *:*(y: F[Boolean]): F[A]
  end extension
end VectorisedField

object VectorisedAdditiveMonoids:

  given additiveArrayMonoidMat: VectorisedAdditiveGroup[Matrix, Double] = new VectorisedAdditiveGroup[Matrix, Double]:
    def empty(hasDim: Matrix[Double]): Matrix[Double] = Matrix.zeros[Double](hasDim.shape)
    def combine(x: Matrix[Double], y: Matrix[Double]): Matrix[Double] = vecxt.all.+(x)(y)
    override def repeatedCombineN(a: Matrix[Double], n: Int): Matrix[Double] = a * n
    // Members declared in io.github.quafadas.inspireRAD.VectorisedAdditiveGroup
    def inverse(a: Matrix[Double]): Matrix[Double] = negate(a)

    extension (a: Matrix[Double])
      inline def negate: Matrix[Double] = a * -1
      inline def sum: Double = vecxt.arrays.sum(a.raw)
      inline def +(b: Matrix[Double]): Matrix[Double] = vecxt.all.+(a)(b)
      inline def -(b: Matrix[Double]): Matrix[Double] = vecxt.all.-(a)(b)
    end extension

  given additiveArrayMonoid: VectorisedAdditiveGroup[NArray, Double] = new VectorisedAdditiveGroup[NArray, Double]:
    def empty(hasDim: NArray[Double]): NArray[Double] = NArray.fill[Double](hasDim.length)(0.0)
    def combine(x: NArray[Double], y: NArray[Double]): NArray[Double] = vecxt.arrays.+(x)(y)
    override def repeatedCombineN(a: NArray[Double], n: Int): NArray[Double] = a * n
    // Members declared in io.github.quafadas.inspireRAD.VectorisedAdditiveGroup
    def inverse(a: NArray[Double]): NArray[Double] = negate(a)

    extension (a: NArray[Double])
      inline def negate: NArray[Double] = a * -1
      inline def sum: Double = vecxt.arrays.sum(a)
      inline def +(b: NArray[Double]): NArray[Double] = vecxt.arrays.+(a)(b)
      inline def -(b: NArray[Double]): NArray[Double] = vecxt.arrays.-(a)(b)
    end extension

    // Members declared in io.github.quafadas.inspireRAD.VectorisedAdditiveSemigroup

  given additiveVectorMonoid: VectorisedMonoid[Vector, Double] = new VectorisedMonoid[Vector, Double]:
    def empty(hasDim: Vector[Double]): Vector[Double] = Vector.fill[Double](hasDim.length)(0.0)
    def combine(x: Vector[Double], y: Vector[Double]): Vector[Double] = x.zip(y).map((a, b) => a + b)
    override def repeatedCombineN(a: Vector[Double], n: Int): Vector[Double] = a.map(_ * n)

  given additiveIntVectorMonoid: VectorisedMonoid[Vector, Int] = new VectorisedMonoid[Vector, Int]:
    def empty(hasDim: Vector[Int]): Vector[Int] = Vector.fill[Int](hasDim.length)(0)
    def combine(x: Vector[Int], y: Vector[Int]): Vector[Int] = x.zip(y).map((a, b) => a + b)
    override def repeatedCombineN(a: Vector[Int], n: Int): Vector[Int] = a.map(_ * n)

end VectorisedAdditiveMonoids

trait VectorisedAdditiveGroup[F[_], @sp(Double) A: Semigroup] extends VectorisedAdditiveMonoid[F, A]:

  /** Find the inverse of `a`.
    *
    * `combine(a, inverse(a))` = `combine(inverse(a), a)` = `empty`.
    *
    * Example:
    * {{{
    * scala> import cats.kernel.instances.int._
    *
    * scala> Group[Int].inverse(5)
    * res0: Int = -5
    * }}}
    */
  def inverse(a: F[A]): F[A]

  /** Remove the element `b` from `a`.
    *
    * Equivalent to `combine(a, inverse(b))`
    *
    * Example:
    * {{{
    * scala> import cats.kernel.instances.int._
    *
    * scala> Group[Int].remove(5, 2)
    * res0: Int = 3
    * }}}
    */
  def remove(a: F[A], b: F[A]): F[A] = combine(a, inverse(b))

  extension (a: F[A])

    inline def -(b: F[A]): F[A]

    inline def negate: F[A]
  end extension

end VectorisedAdditiveGroup

trait VectorisedAdditiveMonoid[F[_], @sp(Double) A: Semigroup]
    extends VectorisedAdditiveSemigroup[F, A]
    with VectorisedMonoid[F, A]:

  inline def zero(a: F[A]) = empty(a)

  extension (a: F[A]) inline def sum: A
  end extension
end VectorisedAdditiveMonoid

trait VectorisedAdditiveSemigroup[F[_], @sp(Int, Long, Float, Double) A] extends Semigroup[F[A]]:
  extension (a: F[A])

    inline def +(b: F[A]): F[A]

  end extension

end VectorisedAdditiveSemigroup
