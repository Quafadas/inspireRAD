package io.github.quafadas.inspireRAD

import scala.reflect.*
import scala.NamedTuple.*
import scala.util.chaining.*
import java.util.UUID
import cats.Show
import cats.syntax.show.toShow
import vecxt.ndarray.NDArray
import vecxt.NDArrayDoubleOps.*
import vecxt.NDArrayReductions.*
import vecxt.ndarrayOps.*
import vecxt.BoundsCheck.DoBoundsCheck.no

// Type-level function to extract gradient types from TejV types
type GradientTypes[V <: Tuple] <: Tuple = V match
  case EmptyTuple       => EmptyTuple
  case TejV[t] *: tail  => NDArray[t] *: GradientTypes[tail]

extension (d: Double)
  def tej(using td: TejVGraph[Double]): TejV[Double] =
    TejV(NDArray.fill[Double](Array(), d))
end extension

extension (arr: Array[Double])
  def tej(using td: TejVGraph[Double]): TejV[Double] =
    TejV(NDArray.fromArray(arr))
end extension

object TejV:

  def apply[T](
      value: NDArray[T]
  )(using td: TejVGraph[T], ops: TejOps[T], sh: Show[NDArray[T]]): TejV[T] =
    new TejV(value).tap(td.addToGraph)
  end apply

  def createDontAddToGraph[T](
      value: NDArray[T]
  )(using ops: TejOps[T], sh: Show[NDArray[T]]): TejV[T] =
    new TejV(value)

end TejV

@SerialVersionUID(0L)
final case class TejV[T] private (value: NDArray[T])(using
    ops: TejOps[T],
    sh: Show[NDArray[T]]
) extends Serializable:
  lhs =>
  lazy val id = UUID.randomUUID()

  def exp(using td: TejVGraph[T]): TejV[T] =
    val nv = vecxt.NDArrayDoubleOps.exp(value.asInstanceOf[NDArray[Double]]).asInstanceOf[NDArray[T]]
    new TejV(nv).tap(td.unary(_, UrnaryOps.Exp, lhs.id))
  end exp

  def log(using td: TejVGraph[T]): TejV[T] =
    val nv = vecxt.NDArrayDoubleOps.log(value.asInstanceOf[NDArray[Double]]).asInstanceOf[NDArray[T]]
    new TejV(nv).tap(td.unary(_, UrnaryOps.Log, lhs.id))
  end log

  def sin(using td: TejVGraph[T]): TejV[T] =
    val nv = TejOps.elementwise(value.asInstanceOf[NDArray[Double]], math.sin).asInstanceOf[NDArray[T]]
    new TejV(nv).tap(td.unary(_, UrnaryOps.Sin, lhs.id))
  end sin

  def cos(using td: TejVGraph[T]): TejV[T] =
    val nv = TejOps.elementwise(value.asInstanceOf[NDArray[Double]], math.cos).asInstanceOf[NDArray[T]]
    new TejV(nv).tap(td.unary(_, UrnaryOps.Cos, lhs.id))
  end cos

  def neg(using td: TejVGraph[T]): TejV[T] =
    val nv = vecxt.NDArrayDoubleOps.neg(value.asInstanceOf[NDArray[Double]]).asInstanceOf[NDArray[T]]
    new TejV(nv).tap(td.unary(_, UrnaryOps.Neg, lhs.id))
  end neg

  def +(rhs: TejV[T])(using td: TejVGraph[T]): TejV[T] =
    val nv = (value.asInstanceOf[NDArray[Double]] + rhs.value.asInstanceOf[NDArray[Double]]).asInstanceOf[NDArray[T]]
    new TejV(nv).tap(td.binary(lhs.id, rhs.id, _, BinaryOps.Add))
  end +

  def -(rhs: TejV[T])(using td: TejVGraph[T]): TejV[T] =
    val nv = (value.asInstanceOf[NDArray[Double]] - rhs.value.asInstanceOf[NDArray[Double]]).asInstanceOf[NDArray[T]]
    new TejV(nv).tap(td.binary(lhs.id, rhs.id, _, BinaryOps.Sub))
  end -

  def *(rhs: TejV[T])(using td: TejVGraph[T]): TejV[T] =
    val nv = (value.asInstanceOf[NDArray[Double]] * rhs.value.asInstanceOf[NDArray[Double]]).asInstanceOf[NDArray[T]]
    new TejV(nv).tap(td.binary(lhs.id, rhs.id, _, BinaryOps.Mul))
  end *

  def /(rhs: TejV[T])(using td: TejVGraph[T]): TejV[T] =
    val nv = (value.asInstanceOf[NDArray[Double]] / rhs.value.asInstanceOf[NDArray[Double]]).asInstanceOf[NDArray[T]]
    new TejV(nv).tap(td.binary(lhs.id, rhs.id, _, BinaryOps.Div))
  end /

  /** Divide by a scalar TejV (0-d). Gradient tracked. */
  def div(rhs: TejV[T])(using td: TejVGraph[T]): TejV[T] =
    val scalarD = rhs.value.asInstanceOf[NDArray[Double]].data(rhs.value.asInstanceOf[NDArray[Double]].offset)
    val nv = (value.asInstanceOf[NDArray[Double]] / scalarD).asInstanceOf[NDArray[T]]
    new TejV(nv).tap(td.scalar(lhs.id, rhs.id, _, BinaryScalarOps.Div, rhs.value.asInstanceOf[T]))
  end div

  def clampMin(threshold: T)(using td: TejVGraph[T]): TejV[T] =
    val scalarD = threshold.asInstanceOf[Double]
    val v = value.asInstanceOf[NDArray[Double]]
    val result = NDArray.fill[Double](v.shape, 0.0)
    for i <- 0 until v.numel do
      result.data(result.offset + i) = math.max(v.data(v.offset + i), scalarD)
    // Create a 0-d scalar TejV for the "rhs" (the constant threshold) - we reuse lhs id (no gradient for scalar constant)
    val threshTejId = lhs.id // same id trick: scalar node backward just checks lhs
    new TejV(result.asInstanceOf[NDArray[T]]).tap(tv =>
      td.scalar(lhs.id, lhs.id, tv, BinaryScalarOps.ClampMin, threshold)
    )
  end clampMin

  def sum(using td: TejVGraph[T]): TejV[T] =
    val s = vecxt.NDArrayReductions.sum(value.asInstanceOf[NDArray[Double]])
    val nv = NDArray.fill[Double](Array(), s).asInstanceOf[NDArray[T]]
    TejV.createDontAddToGraph(nv).tap(td.reduction(_, lhs.id, ReductionOps.Sum))
  end sum

  def product(using td: TejVGraph[T]): TejV[T] =
    val s = vecxt.NDArrayReductions.product(value.asInstanceOf[NDArray[Double]])
    val nv = NDArray.fill[Double](Array(), s).asInstanceOf[NDArray[T]]
    TejV.createDontAddToGraph(nv).tap(td.reduction(_, lhs.id, ReductionOps.Product))
  end product

  def mean(using td: TejVGraph[T]): TejV[T] =
    val s = vecxt.NDArrayReductions.mean(value.asInstanceOf[NDArray[Double]])
    val nv = NDArray.fill[Double](Array(), s).asInstanceOf[NDArray[T]]
    TejV.createDontAddToGraph(nv).tap(td.reduction(_, lhs.id, ReductionOps.Mean))
  end mean

  def normaliseRowsL1(using td: TejVGraph[T]): TejV[T] =
    val mat = value.asInstanceOf[NDArray[Double]]
    val rows = mat.shape(0)
    val cols = mat.shape(1)
    val result = NDArray.fill[Double](Array(rows, cols), 0.0)
    for i <- 0 until rows do
      val rowSum = (0 until cols).map(j => math.abs(mat.data(mat.offset + i * mat.strides(0) + j * mat.strides(1)))).sum
      for j <- 0 until cols do
        result.data(result.offset + i * result.strides(0) + j * result.strides(1)) =
          mat.data(mat.offset + i * mat.strides(0) + j * mat.strides(1)) / rowSum
    new TejV(result.asInstanceOf[NDArray[T]]).tap(td.normaliseRows(lhs.id, _, NormaliseRowOps.NormaliseRowsL1))
  end normaliseRowsL1

  def normaliseRowsL2(using td: TejVGraph[T]): TejV[T] =
    val mat = value.asInstanceOf[NDArray[Double]]
    val rows = mat.shape(0)
    val cols = mat.shape(1)
    val result = NDArray.fill[Double](Array(rows, cols), 0.0)
    for i <- 0 until rows do
      val rowL2 = math.sqrt((0 until cols).map(j => {
        val v = mat.data(mat.offset + i * mat.strides(0) + j * mat.strides(1))
        v * v
      }).sum)
      for j <- 0 until cols do
        result.data(result.offset + i * result.strides(0) + j * result.strides(1)) =
          mat.data(mat.offset + i * mat.strides(0) + j * mat.strides(1)) / rowL2
    new TejV(result.asInstanceOf[NDArray[T]]).tap(td.normaliseRows(lhs.id, _, NormaliseRowOps.NormaliseRowsL2))
  end normaliseRowsL2

  def softmaxRows(using td: TejVGraph[T]): TejV[T] =
    val mat = value.asInstanceOf[NDArray[Double]]
    val rows = mat.shape(0)
    val cols = mat.shape(1)
    val result = NDArray.fill[Double](Array(rows, cols), 0.0)
    for i <- 0 until rows do
      val rowMax = (0 until cols).map(j => mat.data(mat.offset + i * mat.strides(0) + j * mat.strides(1))).max
      val exps = (0 until cols).map(j => math.exp(mat.data(mat.offset + i * mat.strides(0) + j * mat.strides(1)) - rowMax))
      val sumExp = exps.sum
      for j <- 0 until cols do
        result.data(result.offset + i * result.strides(0) + j * result.strides(1)) = exps(j) / sumExp
    new TejV(result.asInstanceOf[NDArray[T]]).tap(td.normaliseRows(lhs.id, _, NormaliseRowOps.Softmax))
  end softmaxRows

  /** Matrix multiply (2-d @@ 2-d). */
  def @@(rhs: TejV[T])(using td: TejVGraph[T]): TejV[T] =
    val result = (value.asInstanceOf[NDArray[Double]] @@ rhs.value.asInstanceOf[NDArray[Double]]).asInstanceOf[NDArray[T]]
    new TejV(result).tap(mval => td.matrixy(lhs.id, rhs.id, mval, MatrixyBinaryOps.MatMul))
  end @@

  /** Reduce each row to a scalar (returns 1-d NDArray). */
  def mapRowsToScalar(fct: ReductionOps)(using td: TejVGraph[T]): TejV[T] =
    val mat = value.asInstanceOf[NDArray[Double]]
    val rows = mat.shape(0)
    val cols = mat.shape(1)
    val result = NDArray.fill[Double](Array(rows), 0.0)
    fct match
      case ReductionOps.Sum =>
        for i <- 0 until rows do
          result.data(result.offset + i) =
            (0 until cols).map(j => mat.data(mat.offset + i * mat.strides(0) + j * mat.strides(1))).sum
      case ReductionOps.Product =>
        for i <- 0 until rows do
          result.data(result.offset + i) =
            (0 until cols).map(j => mat.data(mat.offset + i * mat.strides(0) + j * mat.strides(1))).product
      case ReductionOps.Mean =>
        for i <- 0 until rows do
          result.data(result.offset + i) =
            (0 until cols).map(j => mat.data(mat.offset + i * mat.strides(0) + j * mat.strides(1))).sum / cols.toDouble
    new TejV(result.asInstanceOf[NDArray[T]]).tap(td.rowReduction(_, lhs.id, fct))
  end mapRowsToScalar

  /** Select elements at given (row, col) indices from a 2-d matrix. Returns 1-d result. */
  def apply(indices: Array[(Int, Int)])(using td: TejVGraph[T]): TejV[T] =
    val mat = value.asInstanceOf[NDArray[Double]]
    val result = NDArray.fill[Double](Array(indices.length), 0.0)
    for z <- 0 until indices.length do
      val (i, j) = indices(z)
      result.data(result.offset + z) = mat.data(mat.offset + i * mat.strides(0) + j * mat.strides(1))
    new TejV(result.asInstanceOf[NDArray[T]]).tap(tv => td.selectIndicies(tv, indices, lhs.id))
  end apply

  /** Gather values from a 2-d matrix at given (row, col) indices into a flat 1-d array. */
  def arrange(indices: Array[(Int, Int)])(using td: TejVGraph[T]): TejV[T] =
    val mat = value.asInstanceOf[NDArray[Double]]
    val result = NDArray.fill[Double](Array(indices.length), 0.0)
    for z <- 0 until indices.length do
      val (i, j) = indices(z)
      result.data(result.offset + z) = mat.data(mat.offset + i * mat.strides(0) + j * mat.strides(1))
    new TejV(result.asInstanceOf[NDArray[T]]).tap(tv => td.arrange(tv, indices, lhs.id))
  end arrange

  def backward[N <: Tuple, V <: Tuple](wrt: NamedTuple[N, V], debug: Boolean = false)(using
      td: TejVGraph[T],
      ct: ClassTag[T]
  ) =
    if debug || !td.dag.isCompletelyConnected then
      println("Writing graph to graph.dot file as graph not completely connected")
      os.write.over(os.pwd / "graph.dot", td.dag.toGraphviz)
    end if
    assert(td.dag.isCompletelyConnected, "Graph is not completely connected before backward pass.")
    val graph = td.dag.toposort
    val reversed = graph.reverse

    reversed.head.setGradOne

    if debug then println("---> Backward pass for TejV")
    end if

    reversed.foreach { node =>
      if debug then println("node: " + node.graphShow)
      end if
      node.backward
    }

    def extractGradientTuple(t: Tuple): Tuple = t match
      case EmptyTuple => EmptyTuple
      case h *: tail  =>
        val tejv = h.asInstanceOf[TejV[?]]
        val grad = td.dag.getNode(tejv.id).grad
        grad *: extractGradientTuple(tail)

    val gradientTuple = extractGradientTuple(wrt.toTuple)
    if debug then
      println(" Graph after backward pass:--------")
      println(td.dag.toGraphviz)
    end if
    if debug then println("-----------Backward pass complete")
    end if

    gradientTuple.asInstanceOf[NamedTuple[N, GradientTypes[V]]]
  end backward

end TejV

