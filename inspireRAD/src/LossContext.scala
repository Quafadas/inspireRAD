package io.github.quafadas.inspireRAD

import scala.reflect.ClassTag
import narr.NArray
import vecxt.matrix.Matrix
import cats.Show
import vecxt.all.row

trait ShowContext[T, Matrix[T], Array[T], Scalar[T]]:
  given shM: cats.Show[Matrix[T]]
  given shA: cats.Show[Array[T]]
  given shS: cats.Show[Scalar[T]]
end ShowContext

object DetailShow extends ShowContext[Double, Matrix, NArray, Scalar]:

  override given shM: Show[Matrix[Double]] =  new Show[Matrix[Double]] {
    def show(mat: Matrix[Double]): String =
      val rows = for i <- 0 until Math.min(1, mat.rows)
              yield mat
                .row(i)
                .map(s => "%.3f".format(s).reverse.padTo(6, ' ').reverse)
                .mkString(" | ")
            val footer = ("-" * (rows.head.length))
      val detail = (rows :+ footer).mkString("\n")
      s"Matrix (${mat.shape})\n $detail"
  }

  override given shA: Show[Array[Double]] =  new Show[Array[Double]] {
    def show(arr: Array[Double]): String =
      arr.map(v => "%.3f".format(v)).mkString("[", ", ", "]")
  }

  override given shS: Show[Scalar[Double]] =  new Show[Scalar[Double]] {
    def show(scalar: Scalar[Double]): String =
      "%.3f".format(scalar.scalar)
  }

object LiteShow extends ShowContext[Double, Matrix, NArray, Scalar]:

  override given shM: Show[Matrix[Double]] =  new Show[Matrix[Double]] {
    def show(mat: Matrix[Double]): String =
      s"Matrix (${mat.shape})"
  }

  override given shA: Show[Array[Double]] =  new Show[Array[Double]] {
    def show(arr: Array[Double]): String =
      s"Array (${arr.length})"
  }

  override given shS: Show[Scalar[Double]] =  new Show[Scalar[Double]] {
    def show(scalar: Scalar[Double]): String =
      "%.3f".format(scalar.scalar)
  }



trait LossContext[Matrix[_], Array[_], Scalar[_], T]:
  given mOps: Matrixy[Matrix, T]
  given fm: VectorisedField[Matrix, T]
  given fa: VectorisedTrig[Array, T]
  given fas: VectorisedField[Scalar, T]
  given faa: VectorisedField[Array, T]
  given redArr: Reductions[Array, T, 1]
  given redMat: Reductions[Matrix, T, 2]
  given t: VectorisedTrig[Matrix, T]
  given nt: Numeric[T]
  given ct: ClassTag[T]
end LossContext

object LossContext:
  // Convenience given for Double types
  given forDouble: LossContext[Matrix, NArray, Scalar, Double] =
    new LossContext[Matrix, NArray, Scalar, Double]:
      given mOps: Matrixy[Matrix, Double] = Matrixy.matOps
      given fm: VectorisedField[Matrix, Double] = VectorisedField.elementwiseMatrixDoubleField
      given fa: VectorisedTrig[NArray, Double] = VectorisedTrig.vta
      given fas: VectorisedField[Scalar, Double] = VectorisedField.scalarField
      given faa: VectorisedField[NArray, Double] = VectorisedField.elementwiseArrayDoubleField
      given redArr: Reductions[NArray, Double, 1] = Reductions.redArray
      given redMat: Reductions[Matrix, Double, 2] = Reductions.redMat
      given t: VectorisedTrig[Matrix, Double] = VectorisedTrig.vtm
      given nt: Numeric[Double] = summon[Numeric[Double]]
      given ct: ClassTag[Double] = summon[ClassTag[Double]]
end LossContext
