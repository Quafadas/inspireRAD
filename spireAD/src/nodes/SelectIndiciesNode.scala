package io.github.quafadas.spireAD

import vecxt.matrix.Matrix
import java.util.UUID
import cats.syntax.show.*
import cats.Show
import narr.*
import vecxt.all.update
import vecxt.all.apply

case class SelectIndiciesNode[T](
    value1: Matrix[T],
    thisId: UUID,
    depId: UUID,
    indices: NArray[(Int, Int)]
)(using
    vf: VectorisedField[Matrix, T],
    vt: VectorisedTrig[Matrix, T],
    maty: Matrixy[Matrix, T],
    sh: Show[Matrix[T]]
) extends VNode[Matrix, T](value1, thisId):

  override def graphShow: String =
    s"SelectIndiciesNode (id: ${thisId.toString().takeRight(4)}, first 5 indices: ${indices.take(5).mkString(", ")})"

  override def backward[N <: VDimChangeNode[?, ?, T]](using td: TejVGraph[T]): Unit =
    val n = td.dag.getNode(depId).asInstanceOf[VNode[Matrix, T]]
    import vecxt.BoundsCheck.DoBoundsCheck.no
    val zeros = vf.zero(value1)
    indices.foreach { case (i, j) =>
      zeros(i, j) = this.grad(i, j)
    }
    n.grad += zeros

end SelectIndiciesNode
