package io.github.quafadas.spireAD

import vecxt.matrix.Matrix
import vecxt.BoundsCheck.BoundsCheck
import scala.reflect.ClassTag
import narr.*
import spire.*
import spire.implicits.*
import vecxt.all.*
import vecxtensions.SpireExt.*
import spire.math.JetDim
import spire.math.Jet
import scala.annotation.nowarn
import vecxt.RowCol

object Matrixy:

  @nowarn
  inline def doubleJetIsMatrixy(using inline bc: BoundsCheck, jd: JetDim): Matrixy[Matrix, Jet[Double]] =
    new Matrixy[Matrix, Jet[Double]]:

      extension (a: Matrix[Jet[Double]])

        override def apply(
            indexes: NArray[(Int, Int)]
        ): Matrix[Jet[Double]] =
          val newMat = Matrix[Jet[Double]](a.shape, Array.fill(a.raw.length)(0.0).jetArr)
          var i = 0
          while i < indexes.length do
            val rc: RowCol = indexes(i)
            val nextEntry = vecxt.MatrixInstance.apply(a)(rc)
            newMat(indexes(i)) = nextEntry
            i += 1
          end while
          newMat
        end apply

        def matmul(b: Matrix[Jet[Double]]): Matrix[Jet[Double]] = a.@@@(b)

        def mapRows(f: NArray[Jet[Double]] => NArray[Jet[Double]]): Matrix[Jet[Double]] =
          vecxt.all.mapRows(a)(f)

        def mapRowsToScalar(f: NArray[Jet[Double]] => Jet[Double]): NArray[Jet[Double]] =
          vecxt.all.mapRowsToScalar(a)(f).raw

        def transpose: Matrix[Jet[Double]] = vecxt.all.transpose(a)

      end extension
    end new
  end doubleJetIsMatrixy

  // @nowarn
  // inline def doubleTejIsMatrixy(using inline bc: BoundsCheck, jd: TejDim[Double]): Matrixy[Matrix, Tej[Double]] =
  //   new Matrixy[Matrix, Tej[Double]]:

  //     extension (a: Matrix[Tej[Double]])

  //       override def apply(
  //           indexes: NArray[(Int, Int)]
  //       ): Matrix[Tej[Double]] =
  //         val newMat = Matrix[Tej[Double]](a.shape, Array.fill(a.raw.length)(0.0).tejArr)
  //         var i = 0
  //         while i < indexes.length do
  //           val rc: RowCol = indexes(i)
  //           val nextEntry = vecxt.MatrixInstance.apply(a)(rc)
  //           newMat(indexes(i)) = nextEntry
  //           i += 1
  //         end while
  //         newMat
  //       end apply

  //       def matmul(b: Matrix[Tej[Double]]): Matrix[Tej[Double]] = a.@@@(b)

  //       def mapRows(f: NArray[Tej[Double]] => NArray[Tej[Double]]): Matrix[Tej[Double]] =
  //         vecxt.all.mapRows(a)(f)

  //       def mapRowsToScalar(f: NArray[Tej[Double]] => Tej[Double]): NArray[Tej[Double]] =
  //         vecxt.all.mapRowsToScalar(a)(f).raw

  //       def transpose: Matrix[Tej[Double]] = vecxt.all.transpose(a)

  //     end extension
  //   end new
  // end doubleTejIsMatrixy

  given matOps: Matrixy[Matrix, Double] = doubleMatrix(using vecxt.BoundsCheck.DoBoundsCheck.yes)

  @nowarn
  inline def doubleMatrix(using inline bc: BoundsCheck): Matrixy[Matrix, Double] = new Matrixy[Matrix, Double]:

    extension (a: Matrix[Double])

      override def apply(
          i: NArray[(Int, Int)]
      ): Matrix[Double] =
        val ct = summon[ClassTag[Double]]
        vecxt.MatrixInstance.apply(a)(i)(using bc, ct)
      end apply

      def matmul(b: Matrix[Double]): Matrix[Double] = vecxt.all.@@(a)(b)

      def mapRows(f: NArray[Double] => NArray[Double]): Matrix[Double] =
        vecxt.all.mapRows(a)(f)

      def mapRowsToScalar(f: NArray[Double] => Double): NArray[Double] =
        vecxt.all.mapRowsToScalar(a)(f).raw

      def transpose: Matrix[Double] = vecxt.all.transpose(a)

    end extension

end Matrixy

// This is split out, because once they are in, you are committed to matrix.
trait Matrixy[F[_], A]:

  extension (a: F[A])

    // def apply(i: NArray[Int], j: NArray[Int]): F[A]
    def apply(i: NArray[(Int, Int)]): F[A]

    def matmul(b: F[A]): F[A]
    inline def @@(b: F[A]): F[A] = matmul(b)

    def mapRows(f: NArray[A] => NArray[A]): F[A]

    def mapRowsToScalar(f: NArray[A] => A): NArray[A]

    def transpose: F[A]

  end extension

end Matrixy
