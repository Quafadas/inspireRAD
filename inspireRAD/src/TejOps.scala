package io.github.quafadas.inspireRAD

import vecxt.ndarray.NDArray
import vecxt.BoundsCheck.DoBoundsCheck.no
import scala.reflect.ClassTag
import cats.Show

/** Single typeclass replacing VectorisedField + VectorisedTrig + Reductions + Matrixy.
  * For T=Double, most operations are provided natively by vecxt NDArray ops.
  * This typeclass provides the shape-aware zero/ones factories plus scalar extraction.
  */
trait TejOps[T]:
  def zeros(shape: Array[Int]): NDArray[T]
  def ones(shape: Array[Int]): NDArray[T]
  def fill(shape: Array[Int], value: T): NDArray[T]
  def numel(a: NDArray[T]): Int = a.numel
  def fromDouble(d: Double): T
  def scalarVal(a: NDArray[T]): T

end TejOps

object TejOps:

  /** Allocate a zero-filled NDArray with given shape. */
  def zerosArr(shape: Array[Int]): NDArray[Double] =
    NDArray.fill[Double](shape, 0.0)

  /** Allocate a ones-filled NDArray with given shape. */
  def onesArr(shape: Array[Int]): NDArray[Double] =
    NDArray.fill[Double](shape, 1.0)

  /** Allocate an NDArray filled with a constant value. */
  def fillArr(shape: Array[Int], value: Double): NDArray[Double] =
    NDArray.fill[Double](shape, value)

  /** Element-wise unary operation on a contiguous NDArray[Double]. */
  def elementwise(a: NDArray[Double], f: Double => Double): NDArray[Double] =
    val n = a.numel
    val newData = new Array[Double](n)
    var idx = 0
    while idx < n do
      newData(idx) = f(a.data(a.offset + idx))
      idx += 1
    NDArray(newData, a.shape.clone())
  end elementwise

  given doubleOps: TejOps[Double] = new TejOps[Double]:
    import vecxt.NDArrayDoubleOps.*
    import vecxt.NDArrayReductions.*
    import vecxt.ndarrayOps.*

    override def zeros(shape: Array[Int]): NDArray[Double] = zerosArr(shape)
    override def ones(shape: Array[Int]): NDArray[Double]  = onesArr(shape)
    override def fill(shape: Array[Int], value: Double): NDArray[Double] = fillArr(shape, value)
    override def fromDouble(d: Double): Double = d
    override def scalarVal(a: NDArray[Double]): Double =
      require(a.ndim == 0 || a.numel == 1, "scalarVal called on non-scalar NDArray")
      a.data(a.offset)

  end doubleOps

end TejOps

// Extension to make scalar extraction convenient
extension (a: NDArray[Double])
  def scalarValue: Double =
    require(a.ndim == 0 || a.numel == 1, s"scalarValue called on NDArray with shape ${a.shape.mkString("[",",","]")}")
    a.data(a.offset)

given Show[NDArray[Double]] with
  def show(a: NDArray[Double]): String =
    if a.ndim == 0 then s"Scalar(${a.data(a.offset)})"
    else if a.ndim == 1 then a.data.slice(a.offset, a.offset + a.numel).mkString("[", ", ", "]")
    else
      val rows = a.shape(0)
      val cols = a.shape(1)
      (0 until rows)
        .map(i =>
          (0 until cols)
            .map(j => "%.4f".format(a.data(a.offset + i * a.strides(0) + j * a.strides(1))))
            .mkString(" | ")
        )
        .mkString("\n")
