package io.github.quafadas.inspireRAD

import cats.Show
import vecxt.matrix.Matrix
import vecxt.all.*

object TejVDoubleAlgebra:

  export Matrixy.matOps

  export VectorisedField.elementwiseMatrixDoubleField
  export VectorisedField.elementwiseArrayDoubleField
  export VectorisedField.scalarField

  export VectorisedTrig.vts
  export VectorisedTrig.vta
  export VectorisedTrig.vtm

  export Reductions.redArray
  export Reductions.redMat
  export Reductions.redScalar

  object ShowDetails:

    given Show[Matrix[Double]] with
      def show(matrix: Matrix[Double]): String =
        val rows =
          for i <- 0 until Math.min(1, matrix.rows)
          yield matrix
            .row(i)
            .map(s => "%.3f".format(s).reverse.padTo(6, ' ').reverse)
            .mkString(" | ")
        val footer = ("-" * (rows.head.length))
        (rows :+ footer).mkString("\n")
      end show
    end given

    given Show[Array[Double]] with
      def show(arr: Array[Double]): String =

        arr.map("%.3f".format(_).reverse.padTo(6, ' ').reverse).mkString("[", ", ", "]")
    end given

    given Show[Scalar[Double]] with
      def show(arr: Scalar[Double]): String =
        arr.scalar.toString
    end given
  end ShowDetails

  object ShowLite:
    given Show[Matrix[Double]] with
      def show(matrix: Matrix[Double]): String =
        "Mat"
    end given

    given Show[Array[Double]] with
      def show(arr: Array[Double]): String =
        "arr"
    end given

    given Show[Scalar[Double]] with
      def show(arr: Scalar[Double]): String =
        "%.3f".format(arr.scalar)
    end given
  end ShowLite

end TejVDoubleAlgebra

// class MatrixAlg[F[_], T](using
//     m: Matrixy[F, T],
//     c: ClassTag[T],
//     t: VectorisedTrig[F, T],
//     f: VectorisedField[F, T],
//     r: Reductions[F, T, InferDimension[F]],
//     fi: Field[T]
// ) extends TejVAlgebra[F, T],
//       VectorisedField[F, T],
//       Matrixy[F, T]:
//   export m.*

// end MatrixAlg

// class TejVAlgebra[F[_], T](using
//     c: ClassTag[T],
//     t: VectorisedTrig[F, T],
//     f: VectorisedField[F, T],
//     r: Reductions[F, T, InferDimension[F]],
//     fi: Field[T]
// ) extends VectorisedTrig[F, T],
//       VectorisedField[F, T],
//       Reductions[F, T, InferDimension[F]]:
//   export t.*
//   export r.*
//   export fi.*

//   // export f.`*`

//   export f.`*`
//   export f.`+`
//   export f.`-`
//   export f.`/`
//   export f.*:*
//   export f.one
//   export f.abs
//   export f.numel
//   export f.zero
//   export f.>
//   export f.allOnes

//   export f.clampMin
//   export f.const
//   export f.productExceptSelf
//   export f.sum
//   export f.sign
//   export f.unary_-

//   override val numDimensions: Int = f.numDimensions

// end TejVAlgebra
