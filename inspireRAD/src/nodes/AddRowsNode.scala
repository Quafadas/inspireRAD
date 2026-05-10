package io.github.quafadas.inspireRAD

import java.util.UUID
import cats.syntax.show.*
import cats.Show
import vecxt.ndarray.NDArray
import vecxt.NDArrayDoubleOps.*
import vecxt.NDArrayReductions.*
import vecxt.ndarrayOps.*
import vecxt.BoundsCheck.DoBoundsCheck.no

/** Adds a 1-d bias vector to each row of a 2-d matrix. */
case class AddRowsNode[T](
    value1: NDArray[T],   // 2-d result
    thisId: UUID,
    matId: UUID,
    vecId: UUID
)(using
    ops: TejOps[T],
    sh: Show[NDArray[T]]
) extends VNode[T](value1, thisId):

  override def graphShow: String =
    s"AddRowsNode (id: ${thisId.toString().takeRight(4)}, \n value: \n ${value.show})"

  override def backward(using td: TejVGraph[T]): Unit =
    val matN = td.dag.getNode(matId)
    val vecN = td.dag.getNode(vecId)
    val g = grad.asInstanceOf[NDArray[Double]]
    // matrix gets upstream gradient directly
    matN.grad = (matN.grad.asInstanceOf[NDArray[Double]] + g).asInstanceOf[NDArray[T]]
    // vector gets sum of all rows of gradient
    val rowSum = sum(g)(0)
    vecN.grad = (vecN.grad.asInstanceOf[NDArray[Double]] + rowSum).asInstanceOf[NDArray[T]]

end AddRowsNode
