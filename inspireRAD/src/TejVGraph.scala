package io.github.quafadas.inspireRAD

import scala.reflect.*
import scala.specialized as sp
import java.util.UUID
import cats.Show
import cats.syntax.show.toShow
import vecxt.ndarray.NDArray
import vecxt.BoundsCheck.DoBoundsCheck.no

case class TejVGraph[T: ClassTag]()(using
    ops: TejOps[T],
    sh: Show[NDArray[T]]
):

  final val dag = DAGV[T, VNode[T]]()

  def resetGrads(using ct: ClassTag[T]): Unit =
    dag.getAllNodes.foreach { node =>
      node.setGradZero
    }
  end resetGrads

  def addToGraph(t: TejV[T]): Unit =
    val n = VConstNode(t.value, t.id)
    dag.addNode(n)
  end addToGraph

  def reduction(tv: TejV[T], depId: UUID, op: ReductionOps): Unit =
    val node = ReductionNode[T](tv.value, tv.id, depId, op, dag.getNode(depId).value.shape)
    dag.addNode(node)
    dag.addEdge(depId, tv.id)
  end reduction

  def unary(tv: TejV[T], op: UrnaryOps, depId: UUID): Unit =
    val node = UrnaryNode[T](op, tv.value, tv.id, depId)
    dag.addNode(node)
    dag.addEdge(depId, tv.id)
  end unary

  def graphDebug(s: String) =
    os.write.over(os.Path("/Users/simon/Code/spire_AD/") / "graph.dot", s)

  def binary(lhs: UUID, rhs: UUID, tv: TejV[T], op: BinaryOps): Unit =
    val node = BinaryNode[T](op, tv.value, tv.id, lhs, rhs)
    dag.addNode(node)
    dag.addEdge(lhs, tv.id)
    dag.addEdge(rhs, tv.id)
  end binary

  def scalar(lhs: UUID, rhs: UUID, tv: TejV[T], op: BinaryScalarOps, scalarVal: T): Unit =
    val node = ScalarNode[T](op, tv.value, tv.id, lhs, rhs, scalarVal)
    dag.addNode(node)
    dag.addEdge(lhs, tv.id)
    dag.addEdge(rhs, tv.id)
  end scalar

  def matrixy(lhs: UUID, rhs: UUID, tv: TejV[T], op: MatrixyBinaryOps): Unit =
    val node = MatrixyNode[T](op, tv.value, tv.id, lhs, rhs)
    dag.addNode(node)
    dag.addEdge(lhs, tv.id)
    dag.addEdge(rhs, tv.id)
  end matrixy

  def reductionWithParams(tv: TejV[T], depId: UUID, op: ParameterisedReductionOps): Unit =
    val node = ReductionWithParams[T](op, tv.value, tv.id, depId)
    dag.addNode(node)
    dag.addEdge(depId, tv.id)
  end reductionWithParams

  def rowReduction(tv: TejV[T], depId: UUID, op: ReductionOps): Unit =
    val node = RowReductionNode[T](op, tv.value, tv.id, depId)
    dag.addNode(node)
    dag.addEdge(depId, tv.id)
  end rowReduction

  def normaliseRows(dep: UUID, value: TejV[T], op: NormaliseRowOps): Unit =
    val node = NormaliseRowsNode[T](value.value, value.id, dep, op)
    dag.addNode(node)
    dag.addEdge(dep, value.id)
  end normaliseRows

  def selectIndicies(tv: TejV[T], indicies: Array[(Int, Int)], incomingId: UUID): Unit =
    val node = SelectIndicesNode[T](tv.value, tv.id, incomingId, indicies)
    dag.addNode(node)
    dag.addEdge(incomingId, tv.id)
  end selectIndicies

  def arrange(tv: TejV[T], indicies: Array[(Int, Int)], incomingId: UUID): Unit =
    val node = ArrangeNode[T](tv.value, tv.id, incomingId, indicies)
    dag.addNode(node)
    dag.addEdge(incomingId, tv.id)
  end arrange

  def addRows(tv: TejV[T], matId: UUID, vecId: UUID): Unit =
    val node = AddRowsNode[T](tv.value, tv.id, matId, vecId)
    dag.addNode(node)
    dag.addEdge(matId, tv.id)
    dag.addEdge(vecId, tv.id)
  end addRows

end TejVGraph
