package io.github.quafadas.spireAD

import java.util.UUID
import scala.specialized as sp
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

case class ReductionWithParams[F[_], @sp(Double) T](
    op: ParameterisedReductionOps[InferDimension[F]],
    value1: F[T],
    thisId: UUID,
    depId: UUID,
    singleParam: TupleDim[InferDimension[F]]
)(using
    vf: VectorisedField[F, T],
    vt: VectorisedTrig[F, T],
    reduction: Reductions[F, T, InferDimension[F]],
    sh: Show[F[T]]
) extends VNode[F, T](value1, thisId):

  override def graphShow: String =
    s"ReductionWithParams (id: ${thisId.toString().takeRight(4)}, op: $op, value: ${value.show}, grad: ${grad.show})"

  override def backward[N <: VDimChangeNode[?, ?, T]](using td: TejVGraph[T]): Unit =
    val n = td.dag.getNode(depId).asInstanceOf[VNode[F, T]]
    op match
      case ParameterisedReductionOps.Index(p) =>
        val newGrad = vf.zero(value)
        newGrad(singleParam) = grad(singleParam)
        n.grad = newGrad

      case ParameterisedReductionOps.Update(p) => ???

    end match

  end backward

end ReductionWithParams
