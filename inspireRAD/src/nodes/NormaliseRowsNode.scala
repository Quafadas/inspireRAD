package io.github.quafadas.inspireRAD

import java.util.UUID
import scala.specialized as sp
import cats.syntax.show.*
import cats.Show
import vecxt.matrix.Matrix
import vecxt.MatrixInstance.updateInPlace
import vecxt.all.row
import scala.reflect.ClassTag
import narr.*
import vecxt.BoundsCheck.DoBoundsCheck.yes
import algebra.ring.Field
import vecxt.all.printMat
import vecxt.all.printArr
import NormaliseRowOps.*
import spire.algebra.NRoot

case class NormaliseRowsNode[T](
    value1: Matrix[T],
    thisId: UUID,
    depId: UUID,
    op: NormaliseRowOps
)(using
    vf: VectorisedField[Matrix, T],
    vt: VectorisedTrig[Matrix, T],
    vta: VectorisedField[Array, T],
    red: Reductions[Array, T, 1],
    f: Field[T],
    nr: NRoot[T],
    sh: Show[Matrix[T]],
    numeric: Numeric[T],
    ct: ClassTag[T]
) extends VNode[Matrix, T](value1, thisId):

  override def toString(): String =
    s"v:$value g: $grad \n (_id: ${id.toString().takeRight(4)})"

  override def graphShow: String =
    s"NormaliseRowsNode (id: ${thisId.toString().takeRight(4)}, op: ${op.toString()},\n value: ${value1.show}, \n grad: ${grad.show})"

  override def setGradOne(using ct: ClassTag[T]): Unit =
    grad = vf.allOnes(value1)

  override def backward[N <: VDimChangeNode[?, ?, T]](using td: TejVGraph[T]): Unit =

    val n = td.dag.getNode(depId).asInstanceOf[VDimChangeNode[Matrix, Matrix, T]]
    val newGrad = vf.zero(grad)
    op match
      case Softmax =>
        for i <- 0 until value1.rows do
          val x = value1.row(i)
          val gRow = grad.row(i)
          val dot = red.sum(vta.*(x)(gRow))
          val newGradRow = vta.*(x)(vta.-(gRow)(dot))

          newGrad.updateInPlace(NArray(i), ::, NArray(newGradRow*))
        end for
      case LogSoftmax      => ???
      case NormaliseRowsL1 =>
        // For L1 normalization f(x) = x / ||x||_1, the Jacobian element ∂f_i/∂x_j is:
        // ∂f_i/∂x_j = (δ_ij * ||x||_1 - sign(x_j)) / ||x||_1^2
        // where δ_ij is Kronecker delta and sign(x_j) = ∂|x_j|/∂x_j
        for i <- 0 until value1.rows do
          val x = n.value.row(i)

          val gRow = grad.row(i)
          val norm = red.sum(vta.abs(x))
          if norm != f.zero then
            val normSquared = f.times(norm, norm)
            val signX = vta.sign(x)
            val gSum = vta.sum(gRow)
            val gradRow = x.indices.map { j =>
              val numerator = f.minus(
                f.times(gRow(j), norm),
                f.times(f.times(signX(j), x(j)), gSum)
              )
              f.div(numerator, normSquared)
            }.toArray
            // println("x : " + x.printArr)
            // println("gRow: " + gRow.printArr)
            // println("norm: " + norm)
            // println("normSquared: " + normSquared)
            // println("signX: " + signX.printArr)
            // println("gSum: " + gSum)
            // println("gradRow: " + gradRow.mkString(", "))

            newGrad.updateInPlace(NArray(i), ::, NArray(gradRow*))
          else newGrad.updateInPlace(NArray(i), ::, NArray(vta.zero(x)*))
          end if
      case NormaliseRowOps.NormaliseRowsL2 =>
        // You can express this vectorized per row as:
        // \nabla x = \frac{g \cdot s - \left( \sum_j g_j x_j \right)}{s^2}
        // Or more compactly (in vector form):
        // val dotGX = g dot x
        // val gradX = (g * s - x * dotGX) / (s * s)
        // pre row, where g is a row from the upstream gradient

        for i <- 0 until value1.rows do
          val x = value1.row(i)
          val gRow = grad.row(i)
          val norm = nr.sqrt(red.sum(vta.*(x)(x)))
          val dotGX = red.sum(vta.*(gRow)(x)) // g ⋅ x
          val gS = vta.*(gRow)(norm) // g * s
          val xDotGX = vta.*(x)(dotGX) // x * (g ⋅ x)
          val gradX = vta./(vta.-(gS)(xDotGX))(f.times(norm, norm)) // (g*s - x*(g⋅x)) / s^2
          // println(s"Row $i: x: ${x.printArr}, gRow: ${gRow.printArr}, dotGX: $dotGX, S: $S, S_2: $S_2, gS: ${gS.printArr}")
          // println(s"Row $i: gradX: ${gradX.printArr}")
          // val newRow = vta./(grad.row(i))(gRow)
          newGrad.updateInPlace(NArray(i), ::, NArray(gradX*))
        end for
    end match
    n.grad += newGrad

  end backward
end NormaliseRowsNode
