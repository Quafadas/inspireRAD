package io.github.quafadas.spireAD

import java.util.UUID
import io.github.quafadas.spireAD.ReductionOps
import cats.Show
import scala.reflect.ClassTag
import cats.syntax.all.toShow
import algebra.ring.Field
import io.github.quafadas.spireAD.Tej.zero

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
    val n = td.dag.getNode(depId).asInstanceOf[VNode[F, T]]
    op match
      case ReductionOps.Sum =>
        n.grad = n.vf1.+(n.grad)(n.vf1.allOnes(n.grad))

      case ReductionOps.Product =>
        val pes = n.vf2.productExceptSelf(n.value)()
        n.grad = n.vf2.+(n.grad)(pes)

      case ReductionOps.Mean =>
        n.grad = n.vf2.+(n.grad)(n.vf2./(n.vf2.allOnes(n.grad))(n.vf2.fromDouble(n.vf2.numel(n.value))))

    end match
  end backward

  override def graphShow: String =
    s"ReductionNode (id: ${thisId.toString().takeRight(4)}, value: ${value.show}, grad: ${grad.show})"

end ReductionNode