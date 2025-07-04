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
