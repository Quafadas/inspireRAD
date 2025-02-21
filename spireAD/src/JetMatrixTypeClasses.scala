package io.github.quafadas.spireAD

import spire.*
import spire.math.*
import spire.implicits.*
import spire.algebra.Trig
import _root_.algebra.ring.Field

import vecxt.matrix.Matrix
import _root_.algebra.ring.Ring
import narr.*
import vecxt.BoundsCheck.DoBoundsCheck.yes
import vecxt.all.*
import vecxt.*
import spire.algebra.NRoot
import cats.kernel.Order
import _root_.algebra.ring.Signed

trait JetMatrixTypeClasses[F[_]](using
    r: Ring[F[Double]]
):

  extension (jm: Matrix[F[Double]])
    def @@(x: Matrix[F[Double]])(using
        ordA: Ring[F[Double]],
        ct: ClassTag[F[Double]]
    ): Matrix[F[Double]]

    def sum(using jd: JetDim): F[Double]
    def exp: Matrix[F[Double]]
    def log: Matrix[F[Double]]
    def pt: Unit
    def *(double: Double): Matrix[F[Double]]

  end extension
end JetMatrixTypeClasses

object TejMatrixTypeClasses:

  def ji: TejInstances = new TejInstances {}
  def nrootJet[
      T: NRoot: ClassTag: Field: Order: Signed: Trig
  ](using
      jd: TejDim[T],
      ordA: Ring[Tej[T]]
  ): NRoot[Tej[T]] =
    ji.TejAlgebra[T]

  def matTejTC(using
      ordA: Ring[Tej[Double]],
      jd: TejDim[Double],
      num: Numeric[Double]
  ): JetMatrixTypeClasses[Tej] =
    new JetMatrixTypeClasses[Tej]:
      extension (jm: Matrix[Tej[Double]])

        override def pt: Unit =
          val arrArr =
            for i <- 0 until vecxt.all.rows(jm)
            yield
              val aRow = vecxt.all.row(jm)(i)
              val els =
                for (el <- aRow)
                  yield el.toString()
              els.mkString(" ")

          println(arrArr.mkString("\n"))
        end pt

        override def @@(x: Matrix[Tej[Double]])(using
            ordA: Ring[Tej[Double]],
            ct: ClassTag[Tej[Double]]
        ): Matrix[Tej[Double]] =
          val (r1, c1) = jm.shape
          val (r2, c2) = x.shape

          if c1 != r2 then throw new IllegalArgumentException("Matrix dimensions must agree")
          end if

          val nar = Array.ofDim[Tej[Double]](r1 * c2)
          val res = Matrix(nar, (r1, c2))

          for i <- 0 until r1 do
            for j <- 0 until c2 do
              res((i, j)) = (0 until c1)
                .map { k =>
                  val i1 = jm((i: Row, k: Col))
                  val i2 = x((k: Row, j: Col))
                  i1 * i2
                }
                .reduce(_ + _)
          end for
          res
        end @@

        override def sum(using jd: JetDim): Tej[Double] =
          jm.raw.fold(Tej(0.0))(_ + _)

        override def exp: Matrix[Tej[Double]] =
          Matrix[Tej[Double]](
            jm.raw.map(_.exp),
            jm.shape
          )
        override def log: Matrix[Tej[Double]] =
          Matrix[Tej[Double]](
            jm.raw.map(_.log),
            jm.shape
          )
        override def *(double: Double): Matrix[Tej[Double]] =
          Matrix[Tej[Double]](
            jm.raw.map(_ * double),
            jm.shape
          )
      end extension
end TejMatrixTypeClasses

object JetMatrixTypeClasses:

  def ji: JetInstances = new JetInstances {}
  def nrootJetT(using
      jd: JetDim
  )[T: NRoot: ClassTag: Field: Order: Signed: Trig]: NRoot[Jet[T]] =
    ji.JetAlgebra[T]

  def matJetTC(using ordA: Ring[Jet[Double]]): JetMatrixTypeClasses[Jet] =
    new JetMatrixTypeClasses[Jet]:

      extension (jm: Matrix[Jet[Double]])

        override def log: Matrix[Jet[Double]] =

          Matrix[Jet[Double]](
            jm.raw.map(_.log),
            jm.shape
          )

        override def @@(
            m2: Matrix[Jet[Double]]
        )(using
            ordA: Ring[Jet[Double]],
            ct: ClassTag[Jet[Double]]
        ): Matrix[Jet[Double]] =
          println("enter")
          val (r1, c1) = jm.shape
          val (r2, c2) = m2.shape

          if c1 != r2 then throw new IllegalArgumentException("Matrix dimensions must agree")
          end if

          val nar = Array.ofDim[Jet[Double]](r1 * c2)
          val res = Matrix(nar, (r1, c2))

          for i <- 0 until r1 do
            for j <- 0 until c2 do
              res((i, j)) = (0 until c1)
                .map { k =>
                  val i1 = jm((i: Row, k: Col))
                  val i2 = m2((k: Row, j: Col))
                  i1 * i2
                }
                .reduce(_ + _)
          end for
          res
        end @@

        def sum(using jd: JetDim): Jet[Double] =
          jm.raw.fold(Jet(0.0))(_ + _)

        def exp: Matrix[Jet[Double]] =
          Matrix[Jet[Double]](
            jm.raw.map(_.exp),
            jm.shape
          )

        def *(double: Double): Matrix[Jet[Double]] =
          Matrix[Jet[Double]](
            jm.raw.map(_ * double),
            jm.shape
          )

        def pt: Unit =
          val arrArr =
            for i <- 0 until vecxt.all.rows(jm)
            yield
              val aRow = vecxt.all.row(jm)(i)
              val els =
                for (el <- aRow)
                  yield el.toString()
              els.mkString(" ")

          println(arrArr.mkString("\n"))
        end pt

      end extension
end JetMatrixTypeClasses
