package io.github.quafadas.spireAD

import vecxt.matrix.Matrix
import vecxt.BoundsCheck.BoundsCheck

object Matrixy:

  given matOps: Matrixy[Matrix, Double] = doubleMatrix(using vecxt.BoundsCheck.DoBoundsCheck.yes)

  inline def doubleMatrix(using inline bc: BoundsCheck): Matrixy[Matrix, Double] = new Matrixy[Matrix, Double]:

    extension (a: Matrix[Double])

      def apply(i: Array[Int], j: Array[Int]): Matrix[Double] =
        vecxt.all.apply(a)(i, j)

      def @@(b: Matrix[Double]): Matrix[Double] = vecxt.all.@@(a)(b)

      def mapRows(f: Array[Double] => Array[Double]): Matrix[Double] =
        vecxt.all.mapRows(a)(f)

      def mapRowsToScalar(f: Array[Double] => Double): Matrix[Double] =
        vecxt.all.mapRowsToScalar(a)(f)

    end extension

end Matrixy

// This is split out, because once they are in, you are committed to matrix.
trait Matrixy[F[_], A]:

  extension (a: F[A])

    def apply(i: Array[Int], j: Array[Int]): F[A]

    // inline def @@(b: F[A]): F[A]
    def @@(b: F[A]): F[A]

    def mapRows(f: Array[A] => Array[A]): F[A]

    def mapRowsToScalar(f: Array[A] => A): F[A]
  end extension

end Matrixy
