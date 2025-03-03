package io.github.quafadas.spireAD

import munit.*
import narr.*
import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes
import io.github.quafadas.spireAD.VectorisedTrig
import scala.reflect.ClassTag
// import vecxt.matrix.Matrix
// import vecxt.MatrixHelper.*

class MatrixTCjSuite extends FunSuite:

  test("Matrix Typelcasses") {
    import VectorisedTrig.vtm
    import VectorisedField.elementwiseMatrixDoubleField

    val m = Matrix.fromRows[Double](
      NArray(1.5, 2.0),
      NArray(1.5, 3.0)
    )

    def identityOps[V[_], T](m: V[T])(using f: VectorisedField[V, T], t: VectorisedTrig[V, T], ct: ClassTag[T]): V[T] =
      import f.*
      (m * allOnes(m)).exp.log + zero(m)

    end identityOps

    assertEquals(identityOps(m).raw.toSeq, NArray(1.5, 1.5, 2.0, 3.0).toSeq)

  }

  test("Matrix Typelcasses") {
    import VectorisedTrig.vtm
    import VectorisedField.elementwiseMatrixDoubleField

    val m = Matrix.fromRows[Double](
      NArray(1.5, 2.0),
      NArray(1.5, 3.0)
    )

    def plusOne[V[_], T](m: V[T])(using f: VectorisedField[V, T]): V[T] =
      m + 1.0.const
    end plusOne

    assertEquals(plusOne(m).raw.toSeq, NArray(2.5, 2.5, 3.0, 4.0).toSeq)

  }

end MatrixTCjSuite
