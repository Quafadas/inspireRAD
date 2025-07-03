package io.github.quafadas.spireAD

import scala.collection.mutable
import java.util.UUID
import java.util as ju
import algebra.ring.Field
import spire.algebra.Trig
import scala.reflect.ClassTag
import spire.algebra.NRoot
import scala.specialized as sp

class DAGV[T: ClassTag, N <: VDimChangeNode[?, ?, T]]:
  private val adjacencyList: mutable.Map[UUID, mutable.Set[UUID]] =
    mutable.Map.empty
  private val nodeMap: mutable.Map[UUID, N] = mutable.Map.empty
  private val reverseNodeMap: mutable.Map[N, UUID] = mutable.Map.empty

  def resetGrads(using ct: ClassTag[T]): Unit =
    nodeMap.values.foreach { node =>
      node.setGradZero
    }

  def addNode(node: N): Unit =
    if !adjacencyList.contains(node.id) then
      adjacencyList(node.id) = mutable.Set.empty
      nodeMap(node.id) = node
      reverseNodeMap(node) = node.id

  def addEdge(from: UUID, to: UUID): Unit =
    require(adjacencyList.contains(from), s"Node $from does not exist.")
    require(adjacencyList.contains(to), s"Node $to does not exist.")
    adjacencyList(from) += to
  end addEdge

  def getAllNodes: Set[N] =
    nodeMap.values.toSet

  def removeNode(id: UUID): Unit =
    nodeMap.get(id).foreach(reverseNodeMap -= _)
    adjacencyList -= id
    nodeMap -= id
    adjacencyList.values.foreach(_ -= id)
  end removeNode

  def removeEdge(from: UUID, to: UUID): Unit =
    adjacencyList.get(from).foreach(_ -= to)

  def neighbors(id: UUID): Set[UUID] =
    adjacencyList.getOrElse(id, Set.empty).toSet

  def hasEdge(from: UUID, to: UUID): Boolean =
    adjacencyList.get(from).exists(_.contains(to))

  def isEmpty: Boolean = adjacencyList.isEmpty

  def toposort: List[N] =
    val inDegree = mutable.Map[UUID, Int]().withDefaultValue(0)
    adjacencyList.values.flatten.foreach { id =>
      inDegree(id) += 1
    }

    val zeroInDegreeQueue = mutable.Queue[UUID]()
    adjacencyList.keys.foreach { id =>
      if inDegree(id) == 0 then zeroInDegreeQueue.enqueue(id)
    }

    var sortedList = List[N]()
    while zeroInDegreeQueue.nonEmpty do
      val id = zeroInDegreeQueue.dequeue()
      sortedList ::= nodeMap(id)
      adjacencyList(id).foreach { neighbor =>
        inDegree(neighbor) -= 1
        if inDegree(neighbor) == 0 then zeroInDegreeQueue.enqueue(neighbor)
        end if
      }
    end while

    if sortedList.size != nodeMap.size then throw new IllegalStateException("Graph has at least one cycle")
    end if

    sortedList.reverse
  end toposort

  def getNode(value: UUID): N =
    nodeMap(value)

  inline def toGraphviz: String =
    val sb = new StringBuilder
    sb.append("digraph {\n")

    adjacencyList.foreach { case (node, neighbors) =>
      if neighbors.isEmpty then sb.append(s"  \"${getNode(node).graphShow}\";\n")
      else
        neighbors.foreach { neighbor =>
          sb.append(
            s"  \"${getNode(node).graphShow}\" -> \"${getNode(neighbor).graphShow}\";\n"
          )
        }
    }

    sb.append("}")
    sb.toString()
  end toGraphviz

end DAGV
