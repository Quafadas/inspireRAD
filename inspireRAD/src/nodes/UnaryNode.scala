package io.github.quafadas.inspireRAD

import java.util.UUID
import scala.specialized as sp
import cats.syntax.show.*
import cats.Show
import vecxt.ndarray.NDArray
import vecxt.NDArrayDoubleOps.*
import vecxt.BoundsCheck.DoBoundsCheck.no

case class UrnaryNode[T](
    op: UrnaryOps,
    value1: NDArray[T],
    thisId: UUID,
    depId: UUID
)(using
    ops: TejOps[T],
    sh: Show[NDArray[T]]
) extends VNode[T](value1, thisId):

  override def toString(): String =
    s"$op \n v:$value g: $grad \n (_id: ${id.toString().takeRight(4)})"

  override def graphShow: String =
    s"UrnaryNode (id: ${thisId.toString().takeRight(4)}, op: $op, value: ${value.show}, grad: ${grad.show})"

  override def backward(using td: TejVGraph[T]): Unit =
    val n = td.dag.getNode(depId)

    val g = grad.asInstanceOf[NDArray[Double]]
    val nv = n.value.asInstanceOf[NDArray[Double]]

    val update: NDArray[Double] = op match
      case UrnaryOps.Sin  => g * TejOps.elementwise(nv, math.cos)
      case UrnaryOps.Cos  => neg(g * TejOps.elementwise(nv, math.sin))
      case UrnaryOps.Exp  => g * exp(nv)
      case UrnaryOps.Log  => g / nv
      case UrnaryOps.Neg  => neg(g)
      case _              => ???

    n.grad = (n.grad.asInstanceOf[NDArray[Double]] + update).asInstanceOf[NDArray[T]]

  end backward
end UrnaryNode
