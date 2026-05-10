package io.github.quafadas.inspireRAD

import java.util.UUID
import cats.syntax.show.*
import cats.Show
import vecxt.ndarray.NDArray
import vecxt.NDArrayDoubleOps.*
import vecxt.ndarrayOps.*
import vecxt.BoundsCheck.DoBoundsCheck.no

/** Selects elements from a 2-d matrix at given (row,col) indices into a 1-d array.
  * Backward scatters gradients back to the corresponding matrix positions.
  */
case class SelectIndicesNode[T](
    value1: NDArray[T],   // 1-d result
    thisId: UUID,
    depId: UUID,
    indices: Array[(Int, Int)]
)(using
    ops: TejOps[T],
    sh: Show[NDArray[T]]
) extends VNode[T](value1, thisId):

  override def graphShow: String =
    s"SelectIndicesNode(id: ${thisId.toString().takeRight(4)}, first 5 indices: ${indices.take(5).mkString(", ")})"

  override def backward(using td: TejVGraph[T]): Unit =
    val n = td.dag.getNode(depId)
    val matGrad = n.grad.asInstanceOf[NDArray[Double]]
    val g = grad.asInstanceOf[NDArray[Double]]
    for z <- 0 until indices.length do
      val (i, j) = indices(z)
      matGrad.data(matGrad.offset + i * matGrad.strides(0) + j * matGrad.strides(1)) +=
        g.data(g.offset + z)

end SelectIndicesNode
