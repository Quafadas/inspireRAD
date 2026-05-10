package io.github.quafadas.inspireRAD

import java.util.UUID
import scala.specialized as sp
import cats.syntax.show.*
import cats.Show
import scala.reflect.ClassTag
import algebra.ring.Field
import vecxt.ndarray.NDArray
import vecxt.NDArrayDoubleOps.*
import vecxt.NDArrayReductions.*
import vecxt.BoundsCheck.DoBoundsCheck.no

case class BinaryNode[T](
    op: BinaryOps,
    value1: NDArray[T],
    thisId: UUID,
    left: UUID,
    right: UUID
)(using
    ops: TejOps[T],
    sh: Show[NDArray[T]]
) extends VNode[T](value1, thisId):

  override def toString(): String =
    s"$op \n v:$value1 g: $grad \n (_id: ${id.toString().takeRight(4)})"

  override def graphShow: String =
    s"BinaryNode (id: ${thisId.toString().takeRight(4)}, op: $op \n value: \n ${value.show} \n grad: \n ${grad.show})"

  override def backward(using td: TejVGraph[T]): Unit =
    val leftN = td.dag.getNode(left)
    val rightN = td.dag.getNode(right)

    // Dispatch based on ndim of this node vs. left/right grad shapes
    val thisDim = value1.ndim
    val leftGradDim = leftN.grad.ndim
    val rightGradDim = rightN.grad.ndim

    (thisDim, leftGradDim, rightGradDim) match
      // All same dimension: simple element-wise gradient accumulation
      case (td, ld, rd) if td == ld && td == rd =>
        val g = grad.asInstanceOf[NDArray[Double]]
        val lv = leftN.value.asInstanceOf[NDArray[Double]]
        val rv = rightN.value.asInstanceOf[NDArray[Double]]
        val lg = leftN.grad.asInstanceOf[NDArray[Double]]
        val rg = rightN.grad.asInstanceOf[NDArray[Double]]
        op match
          case BinaryOps.Add =>
            leftN.grad = (lg + g).asInstanceOf[NDArray[T]]
            rightN.grad = (rg + g).asInstanceOf[NDArray[T]]
          case BinaryOps.Sub =>
            leftN.grad = (lg + g).asInstanceOf[NDArray[T]]
            rightN.grad = (rg - g).asInstanceOf[NDArray[T]]
          case BinaryOps.Mul =>
            leftN.grad = (lg + g * rv).asInstanceOf[NDArray[T]]
            rightN.grad = (rg + g * lv).asInstanceOf[NDArray[T]]
          case BinaryOps.Div =>
            leftN.grad = (lg + g / rv).asInstanceOf[NDArray[T]]
            rightN.grad = (rg - g * lv / (rv * rv)).asInstanceOf[NDArray[T]]

      // This node is a scalar (0-d), left/right may have higher dim
      case (0, _, _) =>
        val gradVal = grad.asInstanceOf[NDArray[Double]].data(grad.asInstanceOf[NDArray[Double]].offset)
        val leftScalar = leftN.value.asInstanceOf[NDArray[Double]].data(leftN.value.asInstanceOf[NDArray[Double]].offset)
        val rightScalar = rightN.value.asInstanceOf[NDArray[Double]].data(rightN.value.asInstanceOf[NDArray[Double]].offset)
        val lg = leftN.grad.asInstanceOf[NDArray[Double]]
        val rg = rightN.grad.asInstanceOf[NDArray[Double]]
        op match
          case BinaryOps.Add =>
            leftN.grad = (lg + gradVal).asInstanceOf[NDArray[T]]
            rightN.grad = (rg + gradVal).asInstanceOf[NDArray[T]]
          case BinaryOps.Sub =>
            leftN.grad = (lg + gradVal).asInstanceOf[NDArray[T]]
            rightN.grad = (rg - gradVal).asInstanceOf[NDArray[T]]
          case BinaryOps.Mul =>
            leftN.grad = (lg + gradVal * rightScalar).asInstanceOf[NDArray[T]]
            rightN.grad = (rg + gradVal * leftScalar).asInstanceOf[NDArray[T]]
          case BinaryOps.Div =>
            leftN.grad = (lg + gradVal / rightScalar).asInstanceOf[NDArray[T]]
            rightN.grad = (rg - gradVal * leftScalar / (rightScalar * rightScalar)).asInstanceOf[NDArray[T]]

      case _ =>
        ???

  end backward
end BinaryNode
