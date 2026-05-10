package io.github.quafadas.inspireRAD

import java.util.UUID
import scala.reflect.ClassTag
import cats.Show
import cats.syntax.all.toShow
import vecxt.ndarray.NDArray
import vecxt.NDArrayDoubleOps.*
import vecxt.ndarrayOps.*
import vecxt.BoundsCheck.DoBoundsCheck.no

/** Parameterized index selection: picks a single element from a 1-d or 2-d NDArray.
  * This node selects a scalar (0-d) value from an N-d array.
  */
case class ReductionWithParams[T](
    op: ParameterisedReductionOps,
    value1: NDArray[T],    // 0-d scalar result
    thisId: UUID,
    depId: UUID
)(using
    ops: TejOps[T],
    sh: Show[NDArray[T]]
) extends VNode[T](value1, thisId):

  override def graphShow: String =
    s"ReductionWithParams (id: ${thisId.toString().takeRight(4)}, op: $op, value: ${value.show})"

  override def backward(using td: TejVGraph[T]): Unit =
    val n = td.dag.getNode(depId)
    val gradScalar = grad.asInstanceOf[NDArray[Double]].data(grad.asInstanceOf[NDArray[Double]].offset)
    val newGrad = NDArray.fill[Double](n.value.shape, 0.0)
    op match
      case ParameterisedReductionOps.Index(p) =>
        p match
          case i: Int =>
            newGrad.data(newGrad.offset + i) = gradScalar
          case t: (Int, Int) =>
            val (i, j) = t
            newGrad.data(newGrad.offset + i * newGrad.strides(0) + j * newGrad.strides(1)) = gradScalar
          case _ =>
            throw new UnsupportedOperationException(s"Unsupported index type in ReductionWithParams: $p")
    n.grad = (n.grad.asInstanceOf[NDArray[Double]] + newGrad).asInstanceOf[NDArray[T]]
  end backward

end ReductionWithParams
