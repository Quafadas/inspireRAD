package io.github.quafadas.inspireRAD

import java.util.UUID
import io.github.quafadas.inspireRAD.ReductionOps
import cats.Show
import scala.reflect.ClassTag
import cats.syntax.all.toShow
import algebra.ring.Field
import io.github.quafadas.inspireRAD.Tej.zero

case class ReductionNode[F[_], T](
    value1: Scalar[T],
    thisId: UUID,
    depId: UUID,
    op: ReductionOps,
    gradsize: F[T]
)(using
    vfF: VectorisedField[F, T],
    vfS: VectorisedField[Scalar, T],
    vt: VectorisedTrig[Scalar, T],
    f: Field[T],
    sh: Show[Scalar[T]],
    shF: Show[F[T]],
    ct: ClassTag[T]
) extends VDimChangeNode[Scalar, F, T](value1, thisId):

  var grad: F[T] = vfF.zero(gradsize)

  def setGradOne(using ct: ClassTag[T]): Unit =
    grad = vfF.allOnes(gradsize)

  def setGradZero(using ct: ClassTag[T]): Unit =
    grad = vfF.zero(gradsize)

  override def backward[N <: VDimChangeNode[?, ?, T]](using td: TejVGraph[T]): Unit =
    val n = td.dag.getNode(depId).asInstanceOf[VDimChangeNode[F, F, T]]

    // op match
    //   case ReductionOps.Sum =>
    //     n.grad = n.vf1.+(n.grad)(n.vf1.allOnes(n.grad))

    //   case ReductionOps.Product =>
    //     val pes = n.vf2.productExceptSelf(n.value)()
    //     n.grad = n.vf2.+(n.grad)(pes)

    //   case ReductionOps.Mean =>
    //     n.grad = n.vf2.+(n.grad)(n.vf2./(n.vf2.allOnes(n.grad))(n.vf2.fromDouble(n.vf2.numel(n.value))))

    op match
      case ReductionOps.Sum =>
        val ones = n.vf1.allOnes(n.value)
        // println(n.vf1.+(n.grad)(n.vf1.*(ones)(n.grad)).show)
        n.grad = n.vf1.+(n.grad)(n.vf1.*(ones)(grad)) // broadcast grad

      case ReductionOps.Product =>
        val pes = n.vf2.productExceptSelf(n.value)()
        n.grad = n.vf2.+(n.grad)(n.vf2.*(pes)(grad)) // chain rule

      case ReductionOps.Mean =>
        val meanGrad = n.vf2./(n.vf2.allOnes(n.value))(n.vf2.fromDouble(n.vf2.numel(n.value)))
        n.grad = n.vf2.+(n.grad)(n.vf2.*(meanGrad)(grad)) // broadcast mean grad

    end match
  end backward

  override def graphShow: String =
    s"ReductionNode (id: ${thisId.toString().takeRight(4)}, , op: ${op} \n value: \n ${value.show} \n grad \n ${grad.show})"

end ReductionNode
