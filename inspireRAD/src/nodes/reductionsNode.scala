package io.github.quafadas.inspireRAD

import java.util.UUID
import cats.Show
import cats.syntax.all.toShow
import vecxt.ndarray.NDArray
import vecxt.NDArrayDoubleOps.*
import vecxt.NDArrayReductions.*
import vecxt.ndarrayOps.*
import vecxt.BoundsCheck.DoBoundsCheck.no

/** Reduction from any NDArray to a 0-d (scalar) NDArray. */
case class ReductionNode[T](
    value1: NDArray[T],  // 0-d scalar result
    thisId: UUID,
    depId: UUID,
    op: ReductionOps,
    gradsizeShape: Array[Int] // shape of the input (for allocating grad)
)(using
    ops: TejOps[T],
    sh: Show[NDArray[T]]
) extends VNode[T](value1, thisId):

  override def backward(using td: TejVGraph[T]): Unit =
    val n = td.dag.getNode(depId)
    val gradScalar = grad.asInstanceOf[NDArray[Double]].data(grad.asInstanceOf[NDArray[Double]].offset)

    op match
      case ReductionOps.Sum =>
        // upstream grad = broadcast of scalar grad to input shape
        val ones = ops.ones(n.value.shape)
        val update = (ones.asInstanceOf[NDArray[Double]] * gradScalar).asInstanceOf[NDArray[T]]
        n.grad = (n.grad.asInstanceOf[NDArray[Double]] + update.asInstanceOf[NDArray[Double]]).asInstanceOf[NDArray[T]]

      case ReductionOps.Product =>
        // grad_i = grad * (product / x_i) for each element
        val nv = n.value.asInstanceOf[NDArray[Double]]
        val prod = vecxt.NDArrayReductions.product(nv)
        val pe = nv.data.zipWithIndex.map { (v, i) =>
          val idx = n.value.asInstanceOf[NDArray[Double]].offset + i
          if nv.data(idx) != 0.0 then prod / nv.data(idx) * gradScalar
          else 0.0
        }
        val update = NDArray.fromArray(pe)
        n.grad = (n.grad.asInstanceOf[NDArray[Double]] + update).asInstanceOf[NDArray[T]]

      case ReductionOps.Mean =>
        val count = n.value.numel.toDouble
        val meanGrad = gradScalar / count
        val ones = ops.ones(n.value.shape)
        val update = (ones.asInstanceOf[NDArray[Double]] * meanGrad).asInstanceOf[NDArray[T]]
        n.grad = (n.grad.asInstanceOf[NDArray[Double]] + update.asInstanceOf[NDArray[Double]]).asInstanceOf[NDArray[T]]

  end backward

  override def graphShow: String =
    s"ReductionNode (id: ${thisId.toString().takeRight(4)}, op: $op \n value: \n ${value.show} \n grad \n ${grad.show})"

end ReductionNode

/** Row-wise reduction from a 2-d NDArray to a 1-d NDArray (one scalar per row). */
case class RowReductionNode[T](
    op: ReductionOps,
    value1: NDArray[T],  // 1-d result (one value per row)
    thisId: UUID,
    depId: UUID
)(using
    ops: TejOps[T],
    sh: Show[NDArray[T]]
) extends VNode[T](value1, thisId):

  override def backward(using td: TejVGraph[T]): Unit =
    val n = td.dag.getNode(depId)
    val inMat = n.value.asInstanceOf[NDArray[Double]]
    val rows = inMat.shape(0)
    val cols = inMat.shape(1)
    val inGrad = grad.asInstanceOf[NDArray[Double]]
    val newGrad = NDArray.fill[Double](Array(rows, cols), 0.0)

    op match
      case ReductionOps.Sum =>
        for i <- 0 until rows do
          val g = inGrad.data(inGrad.offset + i)
          for j <- 0 until cols do
            newGrad.data(newGrad.offset + i * newGrad.strides(0) + j * newGrad.strides(1)) = g

      case ReductionOps.Product =>
        for i <- 0 until rows do
          val g = inGrad.data(inGrad.offset + i)
          val rowVals = Array.tabulate(cols)(j => inMat.data(inMat.offset + i * inMat.strides(0) + j * inMat.strides(1)))
          val prod = rowVals.product
          for j <- 0 until cols do
            val v = rowVals(j)
            val pes = if v != 0.0 then prod / v else 0.0
            newGrad.data(newGrad.offset + i * newGrad.strides(0) + j * newGrad.strides(1)) = pes * g

      case ReductionOps.Mean =>
        for i <- 0 until rows do
          val g = inGrad.data(inGrad.offset + i) / cols.toDouble
          for j <- 0 until cols do
            newGrad.data(newGrad.offset + i * newGrad.strides(0) + j * newGrad.strides(1)) = g

    end match

    n.grad = (n.grad.asInstanceOf[NDArray[Double]] + newGrad).asInstanceOf[NDArray[T]]

  end backward

  override def graphShow: String =
    s"RowReductionNode (id: ${thisId.toString().takeRight(4)}, op: $op)"

end RowReductionNode
