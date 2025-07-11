package io.github.quafadas.spireAD

import java.util.UUID
import scala.specialized as sp
import cats.syntax.show.*
import cats.Show
import scala.reflect.ClassTag
import algebra.ring.Field

/** In this class, the scalar constant is always assumed to be on the right side of the operation. It will provide the
  * wrong answers otherwise
  *
  * @param op
  * @param value1
  * @param thisId
  * @param left
  * @param right
  * @param scalar
  * @param inGrad
  * @param vf
  * @param vg
  * @param vt
  * @param n
  * @param f
  * @param shf
  * @param shg
  * @param ct
  */
case class ScalarNode[F[_], G[_], @sp(Double) T](
    op: BinaryScalarOps,
    value1: F[T],
    thisId: UUID,
    left: UUID,
    right: UUID, // a scalar node
    scalar: T,
    inGrad: G[T]
)(using
    vf: VectorisedField[F, T],
    vg: VectorisedField[G, T],
    // vf2: VectorisedField[Scalar, T],
    vt: VectorisedTrig[F, T],
    // rd: Reductions[G, T, InferDimension[G]],
    n: Numeric[T],
    f: Field[T],
    shf: Show[G[T]],
    shg: Show[F[T]],
    ct: ClassTag[T]
) extends VDimChangeNode[F, G, T](value1, thisId):

  var grad: G[T] = vg.zero(inGrad)

  def setGradOne(using ct: scala.reflect.ClassTag[T]): Unit =
    grad = vg.allOnes(grad)
  def setGradZero(using ct: scala.reflect.ClassTag[T]): Unit =
    grad = vg.zero(grad)

  override def toString(): String =
    s"$op \n v:$value1 g: $grad \n (_id: ${id.toString().takeRight(4)})"

  override def graphShow: String =
    s"ScalarNode (id: ${thisId.toString().takeRight(4)}, op: $op \n value: \n ${value1.show} \n grad \n ${grad.show})"

  override def backward[N <: VDimChangeNode[?, ?, T]](using td: TejVGraph[T]): Unit =
    // println("left " + td.dag.getNode(left))
    // println(td.dag.getNode(right))

    // println("grad this node" + this.grad)
    val leftN = td.dag.getNode(left)
    val rightN = td.dag.getNode(right).asInstanceOf[VDimChangeNode[?, Scalar, T]]
    val rvf = rightN.vf2
    val leftGradOps = leftN.vf2
    val leftValOps = leftN.vf1
    val gradDimL = leftN.vf2.numDimensions
    val gradDimR = rightN.vf2.numDimensions
    val thisDim = this.vf2.numDimensions

    op match
      case BinaryScalarOps.Add =>
        // Y = X + c
        // LHS gradient is the same as the input gradient
        leftN.grad = grad.asInstanceOf[leftN.thisGrad[T]]
        // Sum all gradients for the scalar since it affects all elements
        rightN.grad = Scalar(leftN.vf2.sum(leftN.grad))

      case BinaryScalarOps.Sub =>
        ???

      case BinaryScalarOps.Mul =>
        ???

      case BinaryScalarOps.Div =>
        val leftCheat = leftN.asInstanceOf[VDimChangeNode[?, G, T]]
        leftCheat.grad = vf2./(grad)(scalar)
        // val dot = vf.*(value1)(grad.asInstanceOf[F[T]])
        // val sumDot = vf.sum(dot)
        // val toAdd = f.times(f.fromInt(-1), f.div(sumDot, scalar))
        // rightN.grad = rvf.+(rightN.grad)(toAdd)

      case BinaryScalarOps.ClampMin =>
        val leftCheat = leftN.asInstanceOf[VDimChangeNode[F, F, T]]
        val theCheck = vf.>(value1)(scalar)
        val tmp = grad.asInstanceOf[F[T]] *:* theCheck
        leftCheat.grad = tmp

    end match
    // println("--> backward binary" + this.toString())
    // println("Update Left: " + leftN)
    // println("Update Right: " + rightN)
    // println("<--- end backward this node")
  end backward
end ScalarNode
