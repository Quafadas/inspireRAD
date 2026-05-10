package io.github.quafadas.inspireRAD

import java.util.UUID
import scala.specialized as sp
import cats.syntax.show.*
import cats.Show
import scala.reflect.ClassTag
import vecxt.ndarray.NDArray
import vecxt.NDArrayDoubleOps.*
import vecxt.NDArrayReductions.*
import vecxt.ndarrayOps.*
import vecxt.BoundsCheck.DoBoundsCheck.no

/** Handles scalar operations where a constant scalar (not a TejV) is the right-hand operand.
  * Gradient flows to left and right (the right being a scalar TejV with a 0-d NDArray value).
  */
case class ScalarNode[T](
    op: BinaryScalarOps,
    value1: NDArray[T],
    thisId: UUID,
    left: UUID,
    right: UUID,
    scalar: T
)(using
    ops: TejOps[T],
    sh: Show[NDArray[T]]
) extends VNode[T](value1, thisId):

  override def toString(): String =
    s"$op \n v:$value1 g: $grad \n (_id: ${id.toString().takeRight(4)})"

  override def graphShow: String =
    s"ScalarNode (id: ${thisId.toString().takeRight(4)}, op: $op \n value: \n ${value.show} \n grad \n ${grad.show})"

  override def backward(using td: TejVGraph[T]): Unit =
    val leftN = td.dag.getNode(left)
    val rightN = td.dag.getNode(right)
    val g = grad.asInstanceOf[NDArray[Double]]
    val scalarD = scalar.asInstanceOf[Double]

    op match
      case BinaryScalarOps.Add =>
        // Y = X + c, dY/dX = 1, dY/dc = 1 (scalar sum)
        leftN.grad = (leftN.grad.asInstanceOf[NDArray[Double]] + g).asInstanceOf[NDArray[T]]
        val totalGrad = vecxt.NDArrayReductions.sum(g)
        // right is a 0-d scalar NDArray
        rightN.grad = (rightN.grad.asInstanceOf[NDArray[Double]] + totalGrad).asInstanceOf[NDArray[T]]

      case BinaryScalarOps.Sub =>
        leftN.grad = (leftN.grad.asInstanceOf[NDArray[Double]] + g).asInstanceOf[NDArray[T]]
        val totalGrad = vecxt.NDArrayReductions.sum(g)
        rightN.grad = (rightN.grad.asInstanceOf[NDArray[Double]] - totalGrad).asInstanceOf[NDArray[T]]

      case BinaryScalarOps.Mul =>
        leftN.grad = (leftN.grad.asInstanceOf[NDArray[Double]] + g * scalarD).asInstanceOf[NDArray[T]]
        val dotSum = vecxt.NDArrayReductions.sum(g * leftN.value.asInstanceOf[NDArray[Double]])
        rightN.grad = (rightN.grad.asInstanceOf[NDArray[Double]] + dotSum).asInstanceOf[NDArray[T]]

      case BinaryScalarOps.Div =>
        // Y = X / c, dY/dX = 1/c
        leftN.grad = (leftN.grad.asInstanceOf[NDArray[Double]] + g / scalarD).asInstanceOf[NDArray[T]]

      case BinaryScalarOps.ClampMin =>
        // Y = max(X, threshold): grad is 1 where X > threshold, else 0
        val lv = leftN.value.asInstanceOf[NDArray[Double]]
        val mask = lv > scalarD
        val maskedGrad = g * mask.asInstanceOf[NDArray[Double]]  // treat Boolean as Double using map
        // We do a manual element-wise masking since NDArray[Boolean] * NDArray[Double] isn't direct
        val gData = g.data
        val mvData = lv.data
        val result = NDArray.fill[Double](g.shape, 0.0)
        for i <- 0 until g.numel do
          val gi = gData(g.offset + i)
          val vi = mvData(lv.offset + i)
          result.data(result.offset + i) = if vi > scalarD then gi else 0.0
        leftN.grad = (leftN.grad.asInstanceOf[NDArray[Double]] + result).asInstanceOf[NDArray[T]]

    end match
  end backward
end ScalarNode
