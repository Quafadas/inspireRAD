package io.github.quafadas.spireAD

import java.util.UUID
import scala.reflect.ClassTag
import cats.Show
import cats.syntax.all.toShow
import vecxt.matrix.Matrix

case class MapRowsToScalar[F[_], G[_], T](
    value1: F[T],
    incoming: G[T],
    thisId: UUID,
    depId: UUID
)(using
    vf: VectorisedField[F, T],
    vfG: VectorisedField[G, T],
    vt: VectorisedTrig[F, T],
    maty: Matrixy[Matrix, T],
    sh: Show[Array[T]]
) extends VDimChangeNode[F, G, T](value1, thisId):

  var grad: G[T] = vf2.zero(incoming)
  def graphShow: String = "MapRowsToScalar"
  def setGradOne(using ct: ClassTag[T]): Unit = grad = vf2.allOnes(incoming)
  def setGradZero(using ct: ClassTag[T]): Unit = {
      grad = vf2.zero(incoming)
    }
  def backward[N <: VDimChangeNode[?, ?, T]](using td: TejVGraph[T]): Unit =
    val n = td.dag.getNode(depId).asInstanceOf[VNode[G, T]]

  end backward

end MapRowsToScalar

case class ReductionNode[F[_], T](
    value1: F[T],
    thisId: UUID,
    depId: UUID,
    op: ReductionOps
)(using
    vf: VectorisedField[F, T],
    vt: VectorisedTrig[F, T],
    sh: Show[F[T]],
    ct: ClassTag[T]
) extends VNode[F, T](value1, thisId):

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
    s"ReductionNode (id: ${depId.toString().takeRight(4)}, value: ${value.show}, grad: ${grad.show})"

end ReductionNode
