package io.github.quafadas.inspireRAD

import scala.specialized as sp
import vecxt.arrays.*
import cats.kernel.Semigroup
import narr.*
import vecxt.all.*
import vecxt.arrays./

object VectorisedMultiplicativeMonoids:

  given multiplicativeArrayMonoid: VectorisedMultiplicativeGroup[NArray, Double] =
    new VectorisedMultiplicativeGroup[NArray, Double]:
      import vecxt.BoundsCheck.DoBoundsCheck.yes
      def empty(hasDim: NArray[Double]): NArray[Double] = NArray.fill[Double](hasDim.length)(1.0)
      def combine(x: NArray[Double], y: NArray[Double]): NArray[Double] = vecxt.arrays.*(x)(y)

      // Members declared in io.github.quafadas.inspireRAD.VectorisedMultiplicativeGroup
      def inverse(a: NArray[Double]): NArray[Double] = vecxt.arrays./(1.0)(a)

      extension (a: NArray[Double])
        inline def product: Double = vecxt.arrays.product(a)
        inline def *(b: NArray[Double]): NArray[Double] = vecxt.arrays.*(a)(b)
        inline def /(b: NArray[Double]): NArray[Double] = vecxt.arrays./(a)(b)
        inline def reciprocal: NArray[Double] = vecxt.arrays./(1.0)(a)
      end extension

      // Members declared in io.github.quafadas.inspireRAD.VectorisedAdditiveSemigroup

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
