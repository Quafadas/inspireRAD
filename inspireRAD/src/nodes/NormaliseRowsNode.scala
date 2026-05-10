package io.github.quafadas.inspireRAD

import java.util.UUID
import cats.syntax.show.*
import cats.Show
import vecxt.ndarray.NDArray
import vecxt.NDArrayDoubleOps.*
import vecxt.NDArrayReductions.*
import vecxt.ndarrayOps.*
import vecxt.BoundsCheck.DoBoundsCheck.no
import NormaliseRowOps.*

case class NormaliseRowsNode[T](
    value1: NDArray[T],
    thisId: UUID,
    depId: UUID,
    op: NormaliseRowOps
)(using
    ops: TejOps[T],
    sh: Show[NDArray[T]]
) extends VNode[T](value1, thisId):

  override def toString(): String =
    s"v:$value g: $grad \n (_id: ${id.toString().takeRight(4)})"

  override def graphShow: String =
    s"NormaliseRowsNode (id: ${thisId.toString().takeRight(4)}, op: ${op.toString()})"

  override def setGradOne(using ct: scala.reflect.ClassTag[T]): Unit =
    grad = ops.ones(value1.shape)

  override def backward(using td: TejVGraph[T]): Unit =
    val n = td.dag.getNode(depId)
    val inMat = value1.asInstanceOf[NDArray[Double]]
    val g = grad.asInstanceOf[NDArray[Double]]
    val rows = inMat.shape(0)
    val cols = inMat.shape(1)
    val newGrad = NDArray.fill[Double](Array(rows, cols), 0.0)

    def rowSlice(mat: NDArray[Double], i: Int): Array[Double] =
      Array.tabulate(cols)(j => mat.data(mat.offset + i * mat.strides(0) + j * mat.strides(1)))

    def setRowSlice(mat: NDArray[Double], i: Int, row: Array[Double]): Unit =
      for j <- 0 until cols do
        mat.data(mat.offset + i * mat.strides(0) + j * mat.strides(1)) = row(j)

    op match
      case Softmax =>
        for i <- 0 until rows do
          val x = rowSlice(inMat, i)
          val gRow = rowSlice(g, i)
          val dot = x.zip(gRow).map(_ * _).sum
          val newGradRow = x.zip(gRow).map((xi, gi) => xi * (gi - dot))
          setRowSlice(newGrad, i, newGradRow)

      case LogSoftmax => ???

      case NormaliseRowsL1 =>
        for i <- 0 until rows do
          val x = rowSlice(inMat, i)
          val gRow = rowSlice(g, i)
          val norm = x.map(math.abs).sum
          if norm != 0.0 then
            val normSq = norm * norm
            val signX = x.map(v => if v > 0 then 1.0 else if v < 0 then -1.0 else 0.0)
            val gSum = gRow.sum
            val gradRow = x.indices.map { j =>
              (gRow(j) * norm - signX(j) * x(j) * gSum) / normSq
            }.toArray
            setRowSlice(newGrad, i, gradRow)
          else
            setRowSlice(newGrad, i, Array.fill(cols)(0.0))

      case NormaliseRowsL2 =>
        for i <- 0 until rows do
          val x = rowSlice(inMat, i)
          val gRow = rowSlice(g, i)
          val norm = math.sqrt(x.map(v => v * v).sum)
          if norm != 0.0 then
            val dotGX = x.zip(gRow).map(_ * _).sum
            val gradRow = x.indices.map { j =>
              (gRow(j) * norm - x(j) * dotGX) / (norm * norm)
            }.toArray
            setRowSlice(newGrad, i, gradRow)
          else
            setRowSlice(newGrad, i, Array.fill(cols)(0.0))

    end match
    n.grad = (n.grad.asInstanceOf[NDArray[Double]] + newGrad).asInstanceOf[NDArray[T]]

  end backward
end NormaliseRowsNode
