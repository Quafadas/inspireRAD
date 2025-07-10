package io.github.quafadas.spireAD

import java.util.UUID
import scala.specialized as sp
import cats.syntax.show.*
import cats.Show
import scala.reflect.ClassTag
import algebra.ring.Field

case class BinaryNode[F[_], @sp(Double) T](
    op: BinaryOps,
    value1: F[T],
    thisId: UUID,
    left: UUID,
    right: UUID
)(using
    vf: VectorisedField[F, T],
    vt: VectorisedTrig[F, T],
    fi: Field[T],
    sh: Show[F[T]]
) extends VNode[F, T](value1, thisId):

  override def toString(): String =
    s"$op \n v:$value1 g: $grad \n (_id: ${id.toString().takeRight(4)})"

  override def graphShow: String =
    s"BinaryNode (id: ${thisId.toString().takeRight(4)}, op: $op, value: ${value}, grad: ${grad})"

  override def setGradOne(using ct: ClassTag[T]): Unit =
    grad = vf.allOnes(value1)

  override def backward[N <: VDimChangeNode[?, ?, T]](using td: TejVGraph[T]): Unit =
    val leftN = td.dag.getNode(left)
    val rightN = td.dag.getNode(right)
    // println("BinaryNode backward")
    // println("Left Node: " + leftN)
    // println("Right Node: " + rightN)
    val gradDimL = leftN.vf2.numDimensions
    val gradDimR = rightN.vf2.numDimensions
    val thisDim = this.vf2.numDimensions

    // Left and right sides, may have gradients of different dimensions. Arg.
    (thisDim, gradDimL, gradDimR) match
      // If the dimensions match, we can just use the gradients directly
      case (thisDim, gradDimL, gradimR) if (thisDim == gradDimL) && (gradDimR == thisDim) =>
        val leftN2 = leftN.asInstanceOf[VDimChangeNode[F, F, T]]
        val rightN2 = rightN.asInstanceOf[VDimChangeNode[F, F, T]]
        op match
          case BinaryOps.Add =>
            leftN2.grad += leftN2.grad + this.grad
            rightN2.grad += rightN2.grad + this.grad

          case BinaryOps.Sub =>
            leftN2.grad += this.grad
            rightN2.grad -= this.grad

          case BinaryOps.Mul =>
            leftN2.grad += this.grad * rightN2.value
            rightN2.grad += this.grad * leftN2.value

          case BinaryOps.Div =>
            leftN2.grad += this.grad / rightN2.value
            rightN2.grad -= this.grad * leftN2.value / (rightN2.value * rightN2.value)
        end match
      case (thisDim, gradDimL, gradDimR) if (thisDim == 0) =>
        // If this dimension is a scalar, then we can use the scalar operations stored in the left / right nodes.
        // Further, we know that the left and right _values_ are scalar.
        // Left and right _gradients_ however... are not scalar - otherwise we would have the simple case above.
        val gradVal = this.grad.asInstanceOf[Scalar[T]].scalar
        val rightVal = rightN.value.asInstanceOf[Scalar[T]].scalar
        val leftVal = leftN.value.asInstanceOf[Scalar[T]].scalar

        val leftN2 = leftN.vf2
        val rightN2 = rightN.vf2
        op match
          case BinaryOps.Add =>
            leftN.grad = leftN2.+(leftN.grad)(gradVal)
            rightN.grad = rightN2.+(rightN.grad)(gradVal)

          case BinaryOps.Sub =>
            leftN.grad = leftN2.-(leftN.grad)(gradVal)
            rightN.grad = rightN2.-(rightN.grad)(gradVal)

          case BinaryOps.Mul =>
            leftN.grad = leftN2.+(leftN.grad)(fi.times(gradVal, rightVal))
            rightN.grad = rightN2.+(rightN.grad)(fi.times(gradVal, rightVal))

          case BinaryOps.Div =>
            leftN.grad = leftN2.+(leftN.grad)(fi.div(gradVal, rightVal))

            rightN.grad = rightN2.-(rightN.grad)(fi.div(fi.times(gradVal, leftVal), fi.times(rightVal, rightVal)))
        end match
      case (thisDim, gradDimL, gradDimR) =>
        // Let's not think about this right now as it makes my head hurt and I don't need it atm.
        ???
    end match

    // op match
    //   case BinaryOps.Add =>
    //     leftN.grad += leftN.grad + this.grad
    //     rightN.grad += rightN.grad + this.grad

    //   case BinaryOps.Sub =>
    //     leftN.grad += this.grad
    //     rightN.grad -= this.grad

    //   case BinaryOps.Mul =>
    //     leftN.grad += this.grad * rightN.value
    //     rightN.grad += this.grad * leftN.value

    //   case BinaryOps.Div =>
    //     leftN.grad += this.grad / rightN.value
    //     rightN.grad -= this.grad * leftN.value / (rightN.value * rightN.value)

    // end match
    // println("--> backward binary" + this.toString())
    // println("Update Left: " + leftN)
    // println("Update Right: " + rightN)
    // println("<--- end backward this node")
  end backward
end BinaryNode
