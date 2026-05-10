package io.github.quafadas.inspireRAD

import java.util.UUID
import scala.reflect.ClassTag
import cats.Show
import cats.syntax.all.toShow
import vecxt.ndarray.NDArray

/** Base class for all AD graph nodes. value and grad are both NDArray[T].
  * ndim is used instead of "numDimensions" in the old typeclass.
  */
abstract class VNode[T](val value: NDArray[T], val id: UUID)(using
    ops: TejOps[T],
    sh: Show[NDArray[T]]
):
  var grad: NDArray[T] = ops.zeros(value.shape)

  def setGradOne(using ct: ClassTag[T]): Unit =
    grad = ops.ones(value.shape)

  def setGradZero(using ct: ClassTag[T]): Unit =
    grad = ops.zeros(value.shape)

  def backward(using td: TejVGraph[T]): Unit
  def graphShow: String
end VNode

/** A leaf node (input variable) - backward is a no-op. */
case class VConstNode[T](value1: NDArray[T], idIn: UUID)(using
    ops: TejOps[T],
    sh: Show[NDArray[T]]
) extends VNode[T](value1, idIn):

  override def graphShow: String =
    s"VConstNode (id: ${idIn.toString().takeRight(4)} \n value: \n ${value1.show}\n grad: \n${grad.show})"

  override def backward(using td: TejVGraph[T]): Unit = ()
end VConstNode
