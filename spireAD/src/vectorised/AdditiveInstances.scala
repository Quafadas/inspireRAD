package io.github.quafadas.spireAD

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

final case class Scalar[T: Numeric](scalar: T)
object VectorisedField:
  given jetNumeric(using jd: JetDim): Numeric[Jet[Double]] = new Numeric[Jet[Double]]:

    override def parseString(str: String): Option[Jet[Double]] = ???

    override def plus(x: Jet[Double], y: Jet[Double]): Jet[Double] = x + y
    override def minus(x: Jet[Double], y: Jet[Double]): Jet[Double] = x - y
    override def times(x: Jet[Double], y: Jet[Double]): Jet[Double] = x * y
    override def negate(x: Jet[Double]): Jet[Double] = -x
    override def zero: Jet[Double] = Jet.zero[Double]
    override def abs(x: Jet[Double]): Jet[Double] = x.abs // You'll need to implement abs on Jet
    override def signum(x: Jet[Double]): Int = ???

    // Comparison operations
    override def compare(x: Jet[Double], y: Jet[Double]): Int = ???

    // Conversion operations
    override def fromInt(n: Int): Jet[Double] = Jet(n.toDouble)

    override def toInt(x: Jet[Double]): Int = ???
    override def toLong(x: Jet[Double]): Long = ???
    override def toFloat(x: Jet[Double]): Float = ???
    override def toDouble(x: Jet[Double]): Double = ???

  def scalarJetField(using jd: JetDim): VectorisedField[Scalar, Jet[Double]] = new VectorisedField[Scalar, Jet[Double]]:
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
      override def *(y: Scalar[Jet[Double]]): Scalar[Jet[Double]] = Scalar(a.scalar * y.scalar)
      @targetName("rhs*")
      override def *(y: Jet[Double]): Scalar[Jet[Double]] = Scalar(a.scalar * y)
      override def /(y: Scalar[Jet[Double]]): Scalar[Jet[Double]] = Scalar(a.scalar / y.scalar)
      @targetName("rhs/")
      override def /(y: Jet[Double]): Scalar[Jet[Double]] = Scalar(a.scalar / y)
    end extension

  def elementwiseMatrixJetField(using jd: JetDim): VectorisedField[Matrix, Jet[Double]] =
    new VectorisedField[Matrix, Jet[Double]]:
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
      def fromDouble(x: Double): Jet[Double] = Jet(x)
      def zero(x: NArray[Jet[Double]]): NArray[Jet[Double]] = NArray.fill[Jet[Double]](x.length)(Jet.zero[Double])
      def one(x: NArray[Jet[Double]])(using ClassTag[Jet[Double]]): NArray[Jet[Double]] =
        NArray.fill[Jet[Double]](x.length)(Jet.one[Double])
      def allOnes(x: NArray[Jet[Double]])(using ClassTag[Jet[Double]]) = one(x)

      extension (a: Double)
        def const: Jet[Double] = Jet(a)
        def /(b: NArray[Jet[Double]]): NArray[Jet[Double]] = b.map(_ / a)
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

  given scalarField: VectorisedField[Scalar, Double] = new VectorisedField[Scalar, Double]:

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
      override def *(y: Scalar[Double]): Scalar[Double] = Scalar(a.scalar * y.scalar)
      @targetName("rhs*")
      override def *(y: Double): Scalar[Double] = Scalar(a.scalar * y)

      override def /(y: Scalar[Double]): Scalar[Double] = Scalar(a.scalar / y.scalar)

      @targetName("rhs/")
      override def /(y: Double): Scalar[Double] = Scalar(a.scalar / y)
    end extension

  given elementwiseMatrixDoubleField: VectorisedField[Matrix, Double] = new VectorisedField[Matrix, Double]:

    // extension (a: Matrix[Double]) override inline def +(x: Matrix[Double]): Matrix[Double] = ???
    // end extension

    // extension (a: Matrix[Double]) override inline def +(x: Double): Matrix[Double] = ???
    // end extension

    // extension (a: Matrix[Double]) override inline def -(x: Double): Matrix[Double] = ???
    // end extension
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

      inline def *(y: Matrix[Double]): Matrix[Double] = vecxt.all.*(a)(y)

      @targetName("rhs*")
      inline def *(y: Double): Matrix[Double] = vecxt.all.*(a)(y)
      inline def /(y: Matrix[Double]): Matrix[Double] = vecxt.all./(a)(y)
      @targetName("rhs/")
      inline def /(y: Double): Matrix[Double] = Matrix[Double](vecxt.arrays./(a.raw)(y), a.shape)

    end extension

  given elementwiseArrayDoubleField: VectorisedField[NArray, Double] = new VectorisedField[NArray, Double]:
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
  def fromDouble(x: Double): A
  def zero(x: F[A]): F[A]
  def one(x: F[A])(using ClassTag[A]): F[A]
  def allOnes(x: F[A])(using ClassTag[A]): F[A]

  extension (a: Double)
    def const: A
    def /(b: F[A]): F[A]
  end extension

  extension (a: F[A])
    def unary_- : F[A]
    def productExceptSelf(): F[A]
    def numel: Int

    def +(x: F[A]): F[A]
    @targetName("rhs+")
    def +(x: A): F[A]
    def -(y: F[A]): F[A]

    def *(y: F[A]): F[A]
    @targetName("rhs*")
    def *(y: A): F[A]

    def /(y: F[A]): F[A]
    @targetName("rhs/")
    def /(y: A): F[A]
  end extension
end VectorisedField

object VectorisedAdditiveMonoids:

  given additiveArrayMonoidMat: VectorisedAdditiveGroup[Matrix, Double] = new VectorisedAdditiveGroup[Matrix, Double]:
    def empty(hasDim: Matrix[Double]): Matrix[Double] = Matrix.zeros[Double](hasDim.shape)
    def combine(x: Matrix[Double], y: Matrix[Double]): Matrix[Double] = vecxt.all.+(x)(y)
    override def repeatedCombineN(a: Matrix[Double], n: Int): Matrix[Double] = a * n
    // Members declared in io.github.quafadas.spireAD.VectorisedAdditiveGroup
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
    // Members declared in io.github.quafadas.spireAD.VectorisedAdditiveGroup
    def inverse(a: NArray[Double]): NArray[Double] = negate(a)

    extension (a: NArray[Double])
      inline def negate: NArray[Double] = a * -1
      inline def sum: Double = vecxt.arrays.sum(a)
      inline def +(b: NArray[Double]): NArray[Double] = vecxt.arrays.+(a)(b)
      inline def -(b: NArray[Double]): NArray[Double] = vecxt.arrays.-(a)(b)
    end extension

    // Members declared in io.github.quafadas.spireAD.VectorisedAdditiveSemigroup

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
