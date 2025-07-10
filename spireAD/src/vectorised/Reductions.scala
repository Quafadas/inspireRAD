package io.github.quafadas.spireAD

import scala.reflect.ClassTag
import vecxt.BoundsCheck
import vecxt.all.*
import narr.native.NArray
import spire.math.Jet
import spire.math.JetDim
import spire.implicits.*

object Reductions:

  def vtmJet(using jd: JetDim): Reductions[Matrix, Jet[Double], 2] = new Reductions[Matrix, Jet[Double], 2]:

    extension (a: Matrix[Jet[Double]])

      override def update(i: (Int, Int), scalar: Jet[Double]): Unit =
        vecxt.MatrixInstance.update(a)(i, scalar)(using BoundsCheck.DoBoundsCheck.yes)

      def mean: Jet[Double] = a.raw.foldLeft(Jet.zero[Double])(_ + _) / Jet(a.raw.length.toDouble)
      def product: Jet[Double] = a.raw.foldLeft(Jet.one[Double])(_ * _)
      inline def sum: Jet[Double] = a.raw.foldLeft(Jet.zero[Double])(_ + _)

      inline def apply(i: (Int, Int)): Jet[Double] =
        vecxt.MatrixInstance.apply(a)(i)(using BoundsCheck.DoBoundsCheck.yes)

    end extension

  def vtaJet(using jd: JetDim): Reductions[NArray, Jet[Double], 1] = new Reductions[NArray, Jet[Double], 1]:

    extension (a: NArray[Jet[Double]])

      override def apply(i: Tuple1[Int]): Jet[Double] = a(i.head)
      override def update(i: Tuple1[Int], scalar: Jet[Double]): Unit = a(i.head) = scalar

      override def product: Jet[Double] = a.foldLeft(Jet.one[Double])(_ * _)
      inline def mean: Jet[Double] = sum / Jet(a.length.toDouble)
      inline def sum: Jet[Double] = a.foldLeft(Jet.zero[Double])(_ + _)

    end extension

  given vta: Reductions[NArray, Double, 1] = new Reductions[NArray, Double, 1]:

    extension (a: NArray[Double])

      override def apply(i: Tuple1[Int]): Double = a(i.head)
      override def update(i: Tuple1[Int], scalar: Double): Unit = a(i.head) = scalar

      override def product: Double = vecxt.all.product(a)
      inline def mean: Double = vecxt.all.mean(a)
      inline def sum: Double = vecxt.all.sum(a)
    end extension

  given vts: Reductions[Scalar, Double, 0] = new Reductions[Scalar, Double, 0]:

    extension (a: Scalar[Double])

      override def apply(i: EmptyTuple): Double = a.scalar
      override def update(i: EmptyTuple, scalar: Double): Unit = ()

      override def product: Double = a.scalar
      inline def mean: Double = a.scalar
      inline def sum: Double = a.scalar

    end extension

  given vtm: Reductions[Matrix, Double, 2] = new Reductions[Matrix, Double, 2]:

    extension (a: Matrix[Double])

      override def update(i: (Int, Int), scalar: Double): Unit =
        vecxt.MatrixInstance.update(a)(i, scalar)(using BoundsCheck.DoBoundsCheck.yes)

      def mean: Double = vecxt.all.sum(a.raw) / a.raw.length
      def product: Double = vecxt.all.product(a.raw)
      inline def sum: Double = vecxt.all.sum(a.raw)

      inline def apply(i: (Int, Int)): Double = vecxt.MatrixInstance.apply(a)(i)(using BoundsCheck.DoBoundsCheck.yes)

    end extension

end Reductions

trait Reductions[F[_], A, N <: Int]:
  extension (a: F[A])
    def sum: A
    def product: A
    def mean: A
    def apply(i: TupleDim[N]): A
    def update(i: TupleDim[N], scalar: A): Unit

  end extension

end Reductions

type TupleDim[N] <: Tuple = N match
  case 0 => EmptyTuple
  case 1 => Tuple1[Int]
  case 2 => Tuple2[Int, Int]
  case 3 => Tuple3[Int, Int, Int]
  case _ => Tuple

type InferDimension[F[_]] = F[?] match
  case Scalar[?] => 0
  case NArray[?] => 1
  case Matrix[?] => 2
  case _         => -1
end InferDimension

type DimDown[N, T] = N match
  case 2 => NArray[T]
  case 1 => Scalar
  case _ => Scalar
end DimDown
