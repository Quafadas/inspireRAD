package io.github.quafadas.spireAD

import java.util.UUID
import scala.specialized as sp
import scala.reflect.ClassTag
import cats.Show
import cats.syntax.all.toShow
import vecxt.matrix.Matrix

trait VNode[F[_], T](forZeroVal: F[T], val id: UUID)(using
    vf: VectorisedField[F, T],
    vt: VectorisedTrig[F, T]
):
  val vf2: VectorisedField[F, T] = vf
  val vt2: VectorisedTrig[F, T] = vt
  val value: F[T]
  var grad: F[T] = vf.zero(forZeroVal)
  def setGradOne(using ct: ClassTag[T]): Unit =
    grad = vf.allOnes(forZeroVal)
  end setGradOne
  def backward[N <: VNode[?, T]](using td: TejVGraph[T]): Unit
  def graphShow: String
end VNode

case class VConstNode[F[_], T](value: F[T], idIn: UUID)(using
    vf: VectorisedField[F, T],
    vt: VectorisedTrig[F, T],
    sh: Show[F[T]]
) extends VNode[F, T](value, idIn):

  override def graphShow: String =
    s"VConstNode (id: ${idIn.toString().takeRight(4)}, const: value: ${value.show}, grad: ${grad.show})"

  override def backward[N <: VNode[?, T]](using td: TejVGraph[T]): Unit = ()

end VConstNode

case class MatrixyNode[T](
    op: MatrixyBinaryOps,
    value: Matrix[T],
    thisId: UUID,
    left: UUID,
    right: UUID
)(using
    vf: VectorisedField[Matrix, T],
    vt: VectorisedTrig[Matrix, T],
    maty: Matrixy[Matrix, T],
    sh: Show[Matrix[T]]
) extends VNode[Matrix, T](value, thisId):

  override def graphShow: String =
    s"MatNode (id: ${thisId.toString().takeRight(4)}, op: $op, value: ${value.show})"

  override def backward[N <: VNode[?, T]](using td: TejVGraph[T]): Unit =
    op match
      case MatrixyBinaryOps.MatMul =>
        val leftN = td.dag.getNode(left).asInstanceOf[MatrixyNode[T]]
        val rightN = td.dag.getNode(right).asInstanceOf[MatrixyNode[T]]
        leftN.grad = vf.+(leftN.grad)(maty.matmul(this.grad)(rightN.value.transpose))
        rightN.grad = vf.+(rightN.grad)(maty.matmul(rightN.value.transpose)(this.grad))

end MatrixyNode

case class ReductionNode[F[_], T](
    value: F[T],
    thisId: UUID,
    depId: UUID,
    op: ReductionOps
)(using
    vf: VectorisedField[F, T],
    vt: VectorisedTrig[F, T],
    sh: Show[F[T]],
    ct: ClassTag[T]
) extends VNode[F, T](value, thisId):

  override def backward[N <: VNode[?, T]](using td: TejVGraph[T]): Unit =
    val n = td.dag.getNode(depId)
    op match
      case ReductionOps.Sum =>
        n.grad = n.vf2.+(n.grad)(n.vf2.allOnes(n.grad))

      case ReductionOps.Product =>
        val pes = n.vf2.productExceptSelf(n.value)()
        n.grad = n.vf2.+(n.grad)(pes)

      case ReductionOps.Mean =>
        n.grad = n.vf2.+(n.grad)(n.vf2./(n.vf2.allOnes(n.grad))(n.vf2.fromDouble(n.vf2.numel(n.value))))

    end match
  end backward

  override def graphShow: String =
    s"ReductionNode (id: ${depId.toString().takeRight(4)}, value: ${value.show}, grad: ${grad.show})"

end ReductionNode

case class ReductionWithParams[F[_], @sp(Double) T](
    op: ParameterisedReductionOps,
    value: F[T],
    thisId: UUID,
    depId: UUID,
    singleParam: TupleDim[InferDimension[F]]
)(using
    vf: VectorisedField[F, T],
    vt: VectorisedTrig[F, T],
    reduction: Reductions[F, T, InferDimension[F]],
    sh: Show[F[T]]
) extends VNode[F, T](value, thisId):

  override def graphShow: String =
    s"ReductionWithParams (id: ${thisId.toString().takeRight(4)}, op: $op, value: ${value.show}, grad: ${grad.show})"

  override def backward[N <: VNode[?, T]](using td: TejVGraph[T]): Unit =
    val n = td.dag.getNode(depId).asInstanceOf[VNode[F, T]]
    op match
      case ParameterisedReductionOps.Index =>
        val newGrad = vf.zero(value)
        newGrad(singleParam) = grad(singleParam)
        n.grad = newGrad

      case ParameterisedReductionOps.Update => ???

    end match

  end backward

end ReductionWithParams

case class UrnaryNode[F[_], @sp(Double) T](
    op: UrnaryOps,
    value: F[T],
    thisId: UUID,
    depId: UUID
)(using
    vf: VectorisedField[F, T],
    vt: VectorisedTrig[F, T],
    sh: Show[F[T]]
) extends VNode[F, T](value, thisId):

  override def toString(): String =
    s"$op \n v:$value g: $grad \n (_id: ${id.toString().takeRight(4)})"

  override def graphShow: String =
    s"UrnaryNode (id: ${thisId.toString().takeRight(4)}, op: $op, value: ${value.show}, grad: ${grad.show})"

  override def backward[N <: VNode[?, T]](using td: TejVGraph[T]): Unit =
    /** Although the ".asInstanceOf[VNode[F, T]]" appears sketchy, it was in fact validated by the compiler, on the
      * _forward_ pass. A failure here would be indicative of a problem building the graph, rather than an
      * incompatibility in the bound
      */

    val n: VNode[F, T] = td.dag.getNode(depId).asInstanceOf[VNode[F, T]]

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
    // println("New grad backward: " + n)
    // println("Updated: by " + update)
    // println("<--- end backward this node")
  end backward
end UrnaryNode

case class BinaryNode[F[_], @sp(Double) T](
    op: BinaryOps,
    value: F[T],
    thisId: UUID,
    left: UUID,
    right: UUID
)(using
    vf: VectorisedField[F, T],
    vt: VectorisedTrig[F, T],
    sh: Show[F[T]]
) extends VNode[F, T](value, thisId):

  override def toString(): String =
    s"$op \n v:$value g: $grad \n (_id: ${id.toString().takeRight(4)})"

  override def graphShow: String =
    s"BinaryNode (id: ${thisId.toString().takeRight(4)}, op: $op, value: ${value.show}, grad: ${grad.show})"

  override def backward[N <: VNode[?, T]](using td: TejVGraph[T]): Unit =
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
