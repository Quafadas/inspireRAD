package io.github.quafadas.spireAD

import java.util.UUID
import scala.specialized as sp
import cats.syntax.show.*
import cats.Show

case class UrnaryNode[F[_], @sp(Double) T](
    op: UrnaryOps,
    value1: F[T],
    thisId: UUID,
    depId: UUID
)(using
    vf: VectorisedField[F, T],
    vt: VectorisedTrig[F, T],
    sh: Show[F[T]]
) extends VNode[F, T](value1, thisId):

  override def toString(): String =
    s"$op \n v:$value g: $grad \n (_id: ${id.toString().takeRight(4)})"

  override def graphShow: String =
    s"UrnaryNode (id: ${thisId.toString().takeRight(4)}, op: $op, value: ${value.show}, grad: ${grad.show})"

  override def backward[N <: VDimChangeNode[?, ?, T]](using td: TejVGraph[T]): Unit =
    /** Although the ".asInstanceOf[VNode[F, T]]" appears sketchy, it was in fact validated by the compiler, on the
      * _forward_ pass. A failure here would be indicative of a problem building the graph, rather than an
      * incompatibility in the bound
      */

    val n: VDimChangeNode[F, F, T] = td.dag.getNode(depId).asInstanceOf[VDimChangeNode[F, F, T]]

    val update = op match
      case UrnaryOps.Sin => this.grad * n.value.cos
      case UrnaryOps.Cos => this.grad * n.value.sin * -1.0.const
      // case UrnaryOps.Tan  => this.grad / (cos(n.realValue) * cos(n.realValue))
      case UrnaryOps.Exp => this.grad * n.value.exp
      case UrnaryOps.Log => this.grad / n.value

      // case UrnaryOps.Sinh => this.grad * cosh(n2.value)
      // case UrnaryOps.Cosh => this.grad * sinh(n.value)
      // case UrnaryOps.Tanh => this.grad / (cosh(n.realValue) * cosh(n.realValue))
      case UrnaryOps.Neg => -this.grad
      // case UrnaryOps.Sqrt => this.grad / (2 * sqrt(n.realValue))
      case _ => ???

    n.grad = n.grad + update
    // n.grad = td.dag.getNode(depId).grad + this.grad
    // println("--->backward unary" + this.toString())
    // println("New grad backward: " + n.id.toString().takeRight(4))
    // println("Updated: by " + update.show)
    // println("Grad: " + n.grad.show)
    // println("<--- end backward this node")

  end backward
end UrnaryNode
