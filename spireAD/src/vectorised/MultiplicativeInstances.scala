package io.github.quafadas.spireAD

import scala.specialized as sp
import vecxt.arrays.*
import cats.kernel.Semigroup

object VectorisedMultiplicativeMonoids:

  given multiplicativeArrayMonoid: VectorisedMultiplicativeGroup[Array, Double] =
    new VectorisedMultiplicativeGroup[Array, Double]:
      import vecxt.BoundsCheck.DoBoundsCheck.yes
      def empty(hasDim: Array[Double]): Array[Double] = Array.fill[Double](hasDim.length)(1.0)
      def combine(x: Array[Double], y: Array[Double]): Array[Double] = vecxt.arrays.*(x)(y)

      // Members declared in io.github.quafadas.spireAD.VectorisedMultiplicativeGroup
      def inverse(a: Array[Double]): Array[Double] = a.map(x => 1 / x)

      extension (a: Array[Double])
        inline def product: Double = vecxt.arrays.product(a)
        inline def *(b: Array[Double]): Array[Double] = vecxt.arrays.*(a)(b)
        inline def /(b: Array[Double]): Array[Double] = vecxt.arrays./(a)(b)
        inline def reciprocal: Array[Double] = a.map(x => 1 / x)
      end extension

      // Members declared in io.github.quafadas.spireAD.VectorisedAdditiveSemigroup

  given multiplicativeVectorMonoid: VectorisedMonoid[Vector, Double] = new VectorisedMonoid[Vector, Double]:
    def empty(hasDim: Vector[Double]): Vector[Double] = Vector.fill[Double](hasDim.length)(0.0)
    def combine(x: Vector[Double], y: Vector[Double]): Vector[Double] = x.zip(y).map((a, b) => a * b)
    override def repeatedCombineN(a: Vector[Double], n: Int): Vector[Double] = a.map(Math.pow(_, n))

  given multiplicativeIntVectorMonoid: VectorisedMonoid[Vector, Int] = new VectorisedMonoid[Vector, Int]:
    def empty(hasDim: Vector[Int]): Vector[Int] = Vector.fill[Int](hasDim.length)(0)
    def combine(x: Vector[Int], y: Vector[Int]): Vector[Int] = x.zip(y).map((a, b) => a * b)
    override def repeatedCombineN(a: Vector[Int], n: Int): Vector[Int] = a.map(_ ^ n)
end VectorisedMultiplicativeMonoids

trait VectorisedMultiplicativeGroup[F[_], @sp(Double) A: Semigroup] extends VectorisedMultiplicativeMonoid[F, A]:

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

    inline def /(b: F[A]): F[A]

    inline def reciprocal: F[A]

  end extension

end VectorisedMultiplicativeGroup

trait VectorisedMultiplicativeMonoid[F[_], @sp(Double) A: Semigroup]
    extends VectorisedMultiplicativeSemigroup[F, A]
    with VectorisedMonoid[F, A]:

  inline def zero(a: F[A]) = empty(a)

  extension (a: F[A]) inline def product: A
  end extension
end VectorisedMultiplicativeMonoid

trait VectorisedMultiplicativeSemigroup[F[_], @sp(Int, Long, Float, Double) A] extends Semigroup[F[A]]:
  extension (a: F[A])

    inline def *(b: F[A]): F[A]

  end extension

end VectorisedMultiplicativeSemigroup
