import spire._
import spire.math._
import spire.implicits._
import spire.algebra.VectorSpace
import _root_.algebra.ring.Field
import spire.algebra.Trig
import spire.syntax.rig
import _root_.algebra.ring.Rig
import spire.std.array.*
import vecxt.all.*
import vecxt.BoundsCheck.DoBoundsCheck.yes

object TejArrayDouble:

  given fieldRig(using td: TejDim[Double]): Field[Array[Double]] =
    new Field[Array[Double]] {

      inline override def negate(x: Array[Double]): Array[Double] = x.map(-_)

      inline override def div(
          x: Array[Double],
          y: Array[Double]
      ): Array[Double] =
        x.zip(y).map((a, b) => a / b)

      inline override def one: Array[Double] = Array.fill(td.dimension)(1.0)

      inline def zero: Array[Double] = Array.fill(td.dimension)(0.0)
      inline def plus(x: Array[Double], y: Array[Double]): Array[Double] =
        x + y
      inline def times(x: Array[Double], y: Array[Double]): Array[Double] =
        x.zip(y).map((a, b) => a * b)
    }

  given vs(using td: TejDim[Double]): VectorSpace[Array[Double], Double] =
    new VectorSpace[Array[Double], Double] {
      def scalar: Field[Double] = Field[Double]
      def timesl(r: Double, v: Array[Double]): Array[Double] = v * r
      def negate(v: Array[Double]): Array[Double] = v.map(-_)
      def zero: Array[Double] = Array.fill(td.dimension)(0.0)
      def plus(x: Array[Double], y: Array[Double]): Array[Double] = x + y
    }
