package io.github.quafadas.inspireRAD

import java.util.UUID
import scala.reflect.ClassTag
import cats.Show
import cats.syntax.all.toShow

trait VDimChangeNode[F[_], G[_], T](val value: F[T], val id: UUID)(using
    vf: VectorisedField[F, T],
    vfG: VectorisedField[G, T],
    vt: VectorisedTrig[F, T],
    shf_ : Show[F[T]],
    shg_ : Show[G[T]]
):
  type thisGrad[T] = G[T]
  type thisValue[T] = F[T]

  var requiredGrad: Boolean = false

  val vf1: VectorisedField[F, T] = vf
  val vf2: VectorisedField[G, T] = vfG
  val shf: Show[F[T]] = shf_
  val shg: Show[G[T]] = shg_

  var grad: G[T]
  def setGradOne(using ct: ClassTag[T]): Unit
  def setGradZero(using ct: ClassTag[T]): Unit
  def backward[N <: VDimChangeNode[?, ?, T]](using td: TejVGraph[T]): Unit
  def graphShow: String
end VDimChangeNode

abstract class VNode[F[_], T](forZeroVal: F[T], id: UUID)(using
    vf: VectorisedField[F, T],
    vt: VectorisedTrig[F, T],
    sh: Show[F[T]]
) extends VDimChangeNode[F, F, T](forZeroVal, id):

  var grad: F[T] = vf.zero(forZeroVal)
  def setGradOne(using ct: ClassTag[T]): Unit =
    grad = vf.allOnes(forZeroVal)
  end setGradOne
  def graphShow: String
  def setGradZero(using ct: ClassTag[T]): Unit =
    grad = vf.zero(forZeroVal)
end VNode

case class VConstNode[F[_], T](value1: F[T], idIn: UUID)(using
    vf: VectorisedField[F, T],
    vt: VectorisedTrig[F, T],
    sh: Show[F[T]]
) extends VNode[F, T](value1, idIn):

  override def graphShow: String =
    s"VConstNode (id: ${idIn.toString().takeRight(4)} \n value: \n ${value1.show}\n grad: \n${grad.show})"

  override def backward[N <: VDimChangeNode[?, ?, T]](using td: TejVGraph[T]): Unit = ()

end VConstNode
