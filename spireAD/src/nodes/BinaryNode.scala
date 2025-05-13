package io.github.quafadas.spireAD

import java.util.UUID
import scala.specialized as sp
import cats.syntax.show.*
import cats.Show

case class BinaryNode[F[_], @sp(Double) T](
    op: BinaryOps,
    value1: F[T],
    thisId: UUID,
    left: UUID,
    right: UUID
)(using
    vf: VectorisedField[F, T],
    vt: VectorisedTrig[F, T],
    sh: Show[F[T]]
) extends VNode[F, T](value1, thisId):

  override def toString(): String =
    s"$op \n v:$value1 g: $grad \n (_id: ${id.toString().takeRight(4)})"

  override def graphShow: String =
    s"BinaryNode (id: ${thisId.toString().takeRight(4)}, op: $op, value: ${value.show}, grad: ${grad.show})"

  override def backward[N <: VDimChangeNode[?, ?, T]](using td: TejVGraph[T]): Unit =
    val leftN = td.dag.getNode(left).asInstanceOf[VNode[F, T]]
    val rightN = td.dag.getNode(right).asInstanceOf[VNode[F, T]]
    op match
      case BinaryOps.Add =>
        leftN.grad += leftN.grad + this.grad
        rightN.grad += rightN.grad + this.grad

      case BinaryOps.Sub =>
        leftN.grad += this.grad
        rightN.grad -= this.grad

      case BinaryOps.Mul =>
        leftN.grad += this.grad * rightN.value
        rightN.grad += this.grad * leftN.value

      case BinaryOps.Div =>
        leftN.grad += this.grad / rightN.value
        rightN.grad -= this.grad * leftN.value / (rightN.value * rightN.value)

    end match
    // println("--> backward binary" + this.toString())
    // println("Update Left: " + leftN)
    // println("Update Right: " + rightN)
    // println("<--- end backward this node")
  end backward
end BinaryNode
