package io.github.quafadas.spireAD

import scala.math.*
import scala.reflect.*

import spire.algebra.*
import spire.syntax.isReal.*
import spire.syntax.vectorSpace.*
import scala.util.chaining.*
import scala.specialized as sp
import java.util.UUID

case class TejVGraph[T: ClassTag]():

  final val dag = DAGV[T, VNode[?, T]]()

  inline def addToGraph[F[_]](t: TejV[F, T])(using
      vf: VectorisedField[F, T],
      vt: VectorisedTrig[F, T]
  ): Unit =
    val n = VConstNode(t.value, t.id)
    dag.addNode(n)
  end addToGraph

  inline def unary[F[_]](tv: TejV[F, T], op: UrnaryOps, depId: UUID)(using
      f: VectorisedField[F, T],
      tr: VectorisedTrig[F, T]
  ): Unit =
    val node = UrnaryNode[F, T](op, tv.value, tv.id, depId)
    dag.addNode(node)
    dag.addEdge(depId, tv.id)
  end unary

  // inline def binary(
  //     lhs: Tej[T],
  //     rhs: Tej[T],
  //     op: TejOpBinary[T]
  // ): Tej[T] =
  //   val l = TejNode(lhs)
  //   val r = TejNode(rhs)
  //   dag.addNode(op)
  //   dag.addEdge(l, op)
  //   dag.addEdge(r, op)
  //   op.value
  // end binary
end TejVGraph

object TejV extends TejInstances:

  def apply[F[_], T](t: F[T])(using td: TejVGraph[T], f: VectorisedField[F, T], tr: VectorisedTrig[F, T]): TejV[F, T] =
    val tn = new TejV(value = t)
    td.addToGraph(tn)
    tn
  end apply

end TejV

@SerialVersionUID(0L)
final case class TejV[F[_], @sp(Float, Double) T] private (value: F[T])(using f: VectorisedField[F, T]):
  lhs =>
  lazy val id = UUID.randomUUID()

  def exp(using t: VectorisedTrig[F, T], td: TejVGraph[T]) =
    new TejV(lhs.value.exp).tap(td.unary(_, UrnaryOps.Exp, lhs.id))
  end exp

  def log(using t: VectorisedTrig[F, T], td: TejVGraph[T]) =
    new TejV(lhs.value.log).tap(td.unary(_, UrnaryOps.Log, lhs.id))

  def backward[G[_]](wrt: Set[TejV[G, T]])(using td: TejVGraph[T], ct: ClassTag[T]) =
    val graph = td.dag.toposort
    val reversed = graph.reverse

    reversed.head.setGradOne

    reversed.foreach { node =>
      node.backward
    }
    val ids = wrt.map(_.id)
    td.dag.getAllNodes.filter(n => ids.contains(n.id)).asInstanceOf[Set[VNode[G, T]]]
  end backward

end TejV

trait TejVInstances[F[_], T]:
  implicit def tejVAlgebra(using
      c: ClassTag[T]
  ): TejVAlgebra[F, T] = new TejVAlgebra[F, T] {}

end TejVInstances

class TejVAlgebra[F[_], @sp(Float, Double) T](using
    c: ClassTag[T],
    t: VectorisedTrig[F, T]
) extends VectorisedTrig[F, T]:
  export t.*
end TejVAlgebra
