package io.github.quafadas.inspireRAD

import vecxt.matrix.Matrix
import java.util.UUID
import cats.syntax.show.*
import cats.Show
import narr.*
import vecxt.all.update
import vecxt.all.apply
import scala.reflect.ClassTag
import algebra.ring.Field

case class ArrangeNode[T](
    value1: Array[T],
    thisId: UUID,
    depId: UUID,
    indices: NArray[(Int, Int)]
)(using
    vfa: VectorisedField[Array, T],
    vt: VectorisedTrig[Array, T],
    sh: Show[Array[T]],
    fi: Field[T],
    ct: ClassTag[T]
) extends VNode[Array, T](value1, thisId):

  override def graphShow: String =
    s"SelectIndiciesNode (id: ${thisId.toString().takeRight(4)}, first 5 indices: ${indices.take(5).mkString(", ")})"

  override def backward[N <: VDimChangeNode[?, ?, T]](using td: TejVGraph[T]): Unit =
    val n = td.dag.getNode(depId).asInstanceOf[VNode[Matrix, T]]
    import vecxt.BoundsCheck.DoBoundsCheck.no
    // val zeros = n.vf1.zero( n.grad )
    for z <- 0 until indices.length do
      val (i, j) = indices(z)
      n.grad(i, j) = fi.plus(n.grad(i, j), this.grad(z))
    end for
    // n.grad += zeros

  end backward
end ArrangeNode
