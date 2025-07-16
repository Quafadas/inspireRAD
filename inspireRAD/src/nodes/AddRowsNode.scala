package io.github.quafadas.inspireRAD

import java.util.UUID
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

case class AddRowsNode[T](
    value1: Matrix[T],
    thisId: UUID,
    depId: UUID,
    vec: Array[T]
)(using
    vf: VectorisedField[Matrix, T],
    vt: VectorisedTrig[Matrix, T],
    vta: VectorisedField[Array, T],
    red: Reductions[Array, T, 1],
    f: Field[T],
    sh: Show[Matrix[T]],
    numeric: Numeric[T],
    ct: ClassTag[T]
) extends VNode[Matrix, T](value1, thisId):

  override def toString(): String =
    s"v:$value g: $grad \n (_id: ${id.toString().takeRight(4)})"

  override def graphShow: String =
    s"NormaliseRowsNode (id: ${thisId.toString().takeRight(4)}, value: ${value.show}, grad: ${grad.show})"

  override def setGradOne(using ct: ClassTag[T]): Unit =
    grad = vf.allOnes(value1)

  override def backward[N <: VDimChangeNode[?, ?, T]](using td: TejVGraph[T]): Unit =
    val n = td.dag.getNode(depId).asInstanceOf[VNode[Matrix, T]]

    println(s"depId: ${depId.toString()}")

    // You can express this vectorized per row as:
    // \nabla x = \frac{g \cdot s - \left( \sum_j g_j x_j \right)}{s^2}
    // Or more compactly (in vector form):
    // val dotGX = g dot x
    // val gradX = (g * s - x * dotGX) / (s * s)
    // pre row, where g is a row from the upstream gradient
    val newGrad = vf.zero(grad)
    for i <- 0 until value1.rows do
      val x = value1.row(i)
      val gRow = grad.row(i)
      val dotGX = vta.*(gRow.toArray)(x.toArray)
      val x_dotGX = vta.*(x)(dotGX)
      val S = red.sum(x)
      val S_2 = numeric.times(S, S)
      val gS = vta.*(gRow)(S)
      val gradX = vta./(vta.-(gS)(dotGX))(
        S_2
      )
      // println(s"Row $i: x: ${x.printArr}, gRow: ${gRow.printArr}, dotGX: $dotGX, S: $S, S_2: $S_2, gS: ${gS.printArr}")
      // println(s"Row $i: gradX: ${gradX.printArr}")
      // val newRow = vta./(grad.row(i))(gRow)
      newGrad.updateInPlace(NArray(i), ::, NArray(gradX*))
    end for

    // println(s"New Grad: ${newGrad.printMat}")
    n.grad += newGrad

  end backward
end AddRowsNode
