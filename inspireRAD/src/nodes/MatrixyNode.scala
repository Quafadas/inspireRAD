package io.github.quafadas.inspireRAD

import java.util.UUID
import cats.syntax.show.*
import cats.Show
import vecxt.ndarray.NDArray
import vecxt.NDArrayDoubleOps.*
import vecxt.NDArrayReductions.*
import vecxt.ndarrayOps.*
import vecxt.BoundsCheck.DoBoundsCheck.no

case class MatrixyNode[T](
    op: MatrixyBinaryOps,
    value1: NDArray[T],
    thisId: UUID,
    left: UUID,
    right: UUID
)(using
    ops: TejOps[T],
    sh: Show[NDArray[T]]
) extends VNode[T](value1, thisId):

  override def graphShow: String =
    s"MatNode (id: ${thisId.toString().takeRight(4)}, op: $op \n value: \n ${value.show})"

  override def backward(using td: TejVGraph[T]): Unit =
    op match
      case MatrixyBinaryOps.MatMul =>
        val leftN = td.dag.getNode(left)
        val rightN = td.dag.getNode(right)
        val g = grad.asInstanceOf[NDArray[Double]]
        val lv = leftN.value.asInstanceOf[NDArray[Double]]
        val rv = rightN.value.asInstanceOf[NDArray[Double]]
        val lg = leftN.grad.asInstanceOf[NDArray[Double]]
        val rg = rightN.grad.asInstanceOf[NDArray[Double]]
        // dL/dA = dL/dC @@ B^T
        // dL/dB = A^T @@ dL/dC
        leftN.grad = (lg + g @@ T(rv)).asInstanceOf[NDArray[T]]
        rightN.grad = (rg + T(lv) @@ g).asInstanceOf[NDArray[T]]

      case MatrixyBinaryOps.AddToRows =>
        // value = matrix + row_broadcast(vec)
        // gradient w.r.t. matrix is just the upstream gradient (same shape)
        // gradient w.r.t. vec is sum of rows of gradient
        val leftN = td.dag.getNode(left)  // the matrix node
        val rightN = td.dag.getNode(right) // the vector node
        val g = grad.asInstanceOf[NDArray[Double]]
        leftN.grad = (leftN.grad.asInstanceOf[NDArray[Double]] + g).asInstanceOf[NDArray[T]]
        // sum gradient across rows to get vector grad
        val rowSumGrad = sum(g)(0) // sum along axis 0 → 1-d result
        rightN.grad = (rightN.grad.asInstanceOf[NDArray[Double]] + rowSumGrad).asInstanceOf[NDArray[T]]

end MatrixyNode
