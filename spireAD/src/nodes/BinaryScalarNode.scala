package io.github.quafadas.spireAD

import java.util.UUID
import scala.specialized as sp
import cats.syntax.show.*
import cats.Show
import scala.reflect.ClassTag
import algebra.ring.Field

case class BinaryScalarNode[F[_], @sp(Double) T](
    op: BinaryScalarOps,
    value1: F[T],
    thisId: UUID,
    left: UUID,
    right: UUID, // a scalar node
    scalar: T
)(using
    vf: VectorisedField[F, T],
    // vf2: VectorisedField[Scalar, T],
    vt: VectorisedTrig[F, T],
    rd: Reductions[F, T, InferDimension[F]],
    n: Numeric[T],
    f: Field[T],
    sh: Show[F[T]],
    ct: ClassTag[T]
) extends VNode[F, T](value1, thisId):

  override def toString(): String =
    s"$op \n v:$value1 g: $grad \n (_id: ${id.toString().takeRight(4)})"

  override def graphShow: String =
    vf.numDimensions match
      case 0 => s"BinaryScalarNode (id: ${thisId.toString().takeRight(4)}, op: $op, value: ${value1.show}, grad: ${grad.show})"
      case 1 => s"BinaryScalarNode (id: ${thisId.toString().takeRight(4)}, op: $op, value: ${value1.show}, grad: ${grad.show})"
      case 2 => s"BinaryScalarNode (id: ${thisId.toString().takeRight(4)}, op: $op, value: ${value1.show}, grad: ${grad.show})"
      case _ => ???



  override def backward[N <: VDimChangeNode[?, ?, T]](using td: TejVGraph[T]): Unit =
    // println("left " + td.dag.getNode(left))
    // println(td.dag.getNode(right))

    // println("grad this node" + this.grad)
    val leftN = td.dag.getNode(left)
    val rightN = td.dag.getNode(right)
    val rvf = rightN.vf2
    val leftGradOps = leftN.vf2
    val leftValOps = leftN.vf1

    op match
      case BinaryScalarOps.Add =>
        // For vector + scalar: gradient flows through to vector unchanged
        // Scalar accumulates sum of all gradients (since it's broadcast to all elements)
        // leftN.grad = leftGradOps.+(leftN.grad)(scalar)
        // Sum all gradients for the scalar since it affects all elements
        val scalarGrad = this.grad.sum
        rightN.grad = rvf.+(rightN.grad)(scalarGrad)

      case BinaryScalarOps.Sub =>
        ???

      case BinaryScalarOps.Mul =>
        ???

      case BinaryScalarOps.Div =>
        // println("left grad start")
        // println(leftN.grad)
        leftN.grad = leftGradOps./(leftN.grad)(scalar)
        // println("left grad doen")
        val dot = f.times(f.fromInt(-1), vf.*(grad)(leftN.value.asInstanceOf[Scalar[T]].scalar).sum)
        val scale = f.times(scalar, scalar)
        val toAdd = f.div(dot, scale)
        rightN.grad = rvf.+(rightN.grad)(toAdd)

      case BinaryScalarOps.ClampMin =>
        val leftCheat = leftN.asInstanceOf[VDimChangeNode[F, F, T]]
        val theCheck = vf.>(value1)(scalar)
        val tmp = grad *:* theCheck
        leftCheat.grad = tmp

    end match
    // println("--> backward binary" + this.toString())
    // println("Update Left: " + leftN)
    // println("Update Right: " + rightN)
    // println("<--- end backward this node")
  end backward
end BinaryScalarNode
