package io.github.quafadas.spireAD

import java.util.UUID
import io.github.quafadas.spireAD.VectorisedField
import scala.specialized as sp

trait VNode[F[_], T](forZeroVal: F[T], val id: UUID)(using
    vf: VectorisedField[F, T],
    vt: VectorisedTrig[F, T]
):
  val vf2: VectorisedField[F, T] = vf
  val vt2: VectorisedTrig[F, T] = vt
  val value: F[T]
  var grad: F[T] = vf.zero(forZeroVal)
  def backward[N <: VNode[?, T]](using td: TejVGraph[T]): Unit
end VNode

case class VConstNode[F[_], T](value: F[T], idIn: UUID)(using
    vf: VectorisedField[F, T],
    vt: VectorisedTrig[F, T]
) extends VNode[F, T](value, idIn):
  // var grad: F[T] = vf.zero(value)

  override def toString(): String =
    s"const \n v:$value g: $grad \n (_id: ${id.toString().takeRight(4)})"

  override def backward[N <: VNode[?, T]](using td: TejVGraph[T]): Unit = ()

end VConstNode

case class UrnaryNode[F[_], @sp(Double) T](
    op: UrnaryOps,
    value: F[T],
    thisId: UUID,
    depId: UUID
)(using
    vf: VectorisedField[F, T],
    vt: VectorisedTrig[F, T]
) extends VNode[F, T](value, thisId):

  override def toString(): String =
    s"$op \n v:$value g: $grad \n (_id: ${id.toString().takeRight(4)})"

  override def backward[N <: VNode[?, T]](using td: TejVGraph[T]): Unit =
    /** Although the ".asInstanceOf[VNode[F, T]]" appears sketchy, it was in fact validated by the compiler, on the
      * _forwards_ pass. A failure here would be indicative of a problem building the graph, rather than an
      * incompatibility in the bound
      */

    val n: VNode[F, T] = td.dag.getNode(depId).asInstanceOf[VNode[F, T]]

    val update = op match
      // case UrnaryOps.Sin  => this.grad * n.value.cos
      // case UrnaryOps.Cos  => this.grad * n.value.sin * -1.0.const
      // case UrnaryOps.Tan  => this.grad / (cos(n.realValue) * cos(n.realValue))
      case UrnaryOps.Exp => this.grad * value.exp
      case UrnaryOps.Log => this.grad / value
      // case UrnaryOps.Sinh => this.grad * cosh(n2.value)
      // case UrnaryOps.Cosh => this.grad * sinh(n.value)
      // case UrnaryOps.Tanh => this.grad / (cosh(n.realValue) * cosh(n.realValue))
      // case UrnaryOps.Neg  => -this.grad
      // case UrnaryOps.Sqrt => this.grad / (2 * sqrt(n.realValue))
      case _ => ???

    n.grad = n.grad + update
    // n.grad = td.dag.getNode(depId).grad + this.grad
    println("--->backward unary" + this.toString())
    println("New grad backward: " + n)
    println("Updated: by " + update)
    println("<--- end backward this node")
  end backward
end UrnaryNode
