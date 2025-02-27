package io.github.quafadas.spireAD.vectorised

import vecxt.matrix.Matrix
import vecxt.BoundsCheck.BoundsCheck

object Matrixy:

  given doubleMatrix: Matrixy[Double] = new Matrixy[Double]:

    extension (a: Matrix[Double])

      inline def @@(b: Matrix[Double])(using inline bc: BoundsCheck): Matrix[Double] = vecxt.all.@@(a)(b)

      inline def mapRows(f: Array[Double] => Array[Double])(using inline bc: BoundsCheck): Matrix[Double] =
        vecxt.all.mapRows(a)(f)

      inline def mapRowsToScalar(f: Array[Double] => Double)(using inline bc: BoundsCheck): Matrix[Double] =
        vecxt.all.mapRowsToScalar(a)(f)

    end extension

end Matrixy

// This is split out, because once they are in, you are committed to matrix.
trait Matrixy[A]:

  extension (a: Matrix[A])

    inline def @@(b: Matrix[A])(using inline bc: BoundsCheck): Matrix[A]

    inline def mapRows(f: Array[A] => Array[A])(using inline bc: BoundsCheck): Matrix[A]

    inline def mapRowsToScalar(f: Array[A] => A)(using inline bc: BoundsCheck): Matrix[A]
  end extension

end Matrixy
