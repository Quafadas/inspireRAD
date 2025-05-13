package io.github.quafadas.spireAD

import java.util.UUID
import scala.specialized as sp
import scala.reflect.ClassTag
import cats.Show
import cats.syntax.all.toShow
import vecxt.matrix.Matrix
import algebra.ring.Field

case class ReductionWithParams[F[_], G[_], @sp(Double) T](
    op: ParameterisedReductionOps[InferDimension[F]],
    value1: F[T],
    thisId: UUID,
    depId: UUID,
    singleParam: TupleDim[InferDimension[G]],
    someGtoSetGrad: G[T]
)(using
    gf: VectorisedField[G, T],
    vf: VectorisedField[F, T],
    vt: VectorisedTrig[F, T],
    svf: Field[T],
    reduction: Reductions[G, T, InferDimension[G]],
    shF: Show[F[T]],
    shG: Show[G[T]]
) extends VDimChangeNode[F, G, T](value1, thisId):

  var grad: G[T] =
    gf.zero(someGtoSetGrad)

  def setGradOne(using ct: ClassTag[T]): Unit = gf.allOnes(grad)

  override def graphShow: String =
    s"ReductionWithParams (id: ${thisId.toString().takeRight(4)}, op: $op, value: ${value.show}, grad: ${grad.show})"

  override def backward[N <: VDimChangeNode[?, ?, T]](using td: TejVGraph[T]): Unit =
    val n = td.dag.getNode(depId).asInstanceOf[VNode[G, T]]
    op match
      case ParameterisedReductionOps.Index(p) =>
        val newGrad = gf.zero(grad)
        newGrad(singleParam) = grad(singleParam)
        n.grad = newGrad

      // case ParameterisedReductionOps.Update(p) =>
      //   val newGrad = gf.zero(grad)
      //   newGrad(singleParam) = svf.one

    end match

  end backward

end ReductionWithParams
