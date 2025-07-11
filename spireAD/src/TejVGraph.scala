package io.github.quafadas.spireAD

import scala.math.*
import scala.reflect.*
import scala.NamedTuple.*
import spire.algebra.*
import scala.util.chaining.*
import scala.specialized as sp
import java.util.UUID
import cats.Show
import cats.syntax.show.toShow
import vecxt.matrix.Matrix
import narr.*
import vecxt.all.`/`

type NumDim[F[_]] <: Int =
  F[?] match
    case Array[?]  => 1
    case Matrix[?] => 2
    case _         => 0

case class TejVGraph[T: ClassTag]():

  final val dag = DAGV[T, VDimChangeNode[?, ?, T]]()

  def resetGrads(using ct: ClassTag[T]) =
    dag.getAllNodes.foreach { node =>
      node.setGradZero
    }
  end resetGrads

  inline def addToGraph[F[_]](t: TejV[F, T])(using
      vf: VectorisedField[F, T],
      vt: VectorisedTrig[F, T],
      sh: Show[F[T]]
  ): Unit =
    val n = VConstNode(t.value, t.id)
    dag.addNode(n)
  end addToGraph

  inline def reduction[F[_]](tv: TejV[Scalar, T], incoming: TejV[F, T], op: ReductionOps, depId: UUID)(using
      f: VectorisedField[F, T],
      fS: VectorisedField[Scalar, T],
      f1: Field[T],
      tr: VectorisedTrig[Scalar, T],
      sh: Show[Scalar[T]],
      shF: Show[F[T]],
      ct: ClassTag[T]
  ): Unit =
    val node = ReductionNode[F, T](tv.value, tv.id, depId, op, f.zero(incoming.value))
    dag.addNode(node)
    dag.addEdge(depId, tv.id)
    // assert(dag.isCompletelyConnected, "Graph is not completely connected after adding reduction node")
  end reduction

  inline def unary[F[_]](tv: TejV[F, T], op: UrnaryOps, depId: UUID)(using
      f: VectorisedField[F, T],
      tr: VectorisedTrig[F, T],
      sh: Show[F[T]]
  ): Unit =
    val node = UrnaryNode[F, T](op, tv.value, tv.id, depId)
    dag.addNode(node)
    dag.addEdge(depId, tv.id)
    // assert(dag.isCompletelyConnected, "Graph is not completely connected after adding unary node")
  end unary

  def graphDebug(s: String) =
    os.write.over(os.Path("/Users/simon/Code/spire_AD/") / "graph.dot", s)

  inline def binary[F[_]](
      lhs: UUID,
      rhs: UUID,
      tv: TejV[F, T],
      op: BinaryOps
  )(using
      vf: VectorisedField[F, T],
      tr: VectorisedTrig[F, T],
      f: Field[T],
      sh: Show[F[T]]
  ): Unit =
    val node = BinaryNode[F, T](op, tv.value, tv.id, lhs, rhs)
    dag.addNode(node)
    dag.addEdge(lhs, tv.id)
    dag.addEdge(rhs, tv.id)
    // graphDebug(dag.toGraphviz)
    // assert(dag.isCompletelyConnected, "Graph is not completely connected after adding binary node")
  end binary

  inline def scalar[F[_]](
      lhs: UUID,
      rhs: UUID,
      tv: TejV[F, T],
      op: BinaryScalarOps,
      scalar: T
  )(using
      vf: VectorisedField[F, T],
      // vg: VectorisedField[G, T],
      tr: VectorisedTrig[F, T],
      f: Field[T],
      // rd: Reductions[G, T, InferDimension[G]],
      ct: ClassTag[T],
      n: Numeric[T],
      shf: Show[F[T]]
      // shg: Show[G[T]]
  ): Unit =
    val leftN = dag.getNode(lhs)

    given gradL: VectorisedField[leftN.thisGrad, T] = leftN.vf2.asInstanceOf[VectorisedField[leftN.thisGrad, T]]
    given shg: Show[leftN.thisGrad[T]] = leftN.shg.asInstanceOf[Show[leftN.thisGrad[T]]]
    val node = ScalarNode[F, leftN.thisGrad, T](op, tv.value, tv.id, lhs, rhs, scalar, leftN.grad)
    dag.addNode(node)
    dag.addEdge(lhs, tv.id)
    dag.addEdge(rhs, tv.id)

    // assert(dag.isCompletelyConnected, "Graph is not completely connected after adding scalar node")

  end scalar

  inline def matrixy(
      lhs: UUID,
      rhs: UUID,
      tv: TejV[Matrix, T],
      op: MatrixyBinaryOps
  )(using
      vfM: VectorisedField[Matrix, T],
      vfA: VectorisedField[Array, T],
      tr: VectorisedTrig[Matrix, T],
      f: Field[T],
      mty: Matrixy[Matrix, T],
      sh: Show[Matrix[T]]
  ): Unit =
    val node = MatrixyNode[T](op, tv.value, tv.id, lhs, rhs)
    dag.addNode(node)
    dag.addEdge(lhs, tv.id)
    dag.addEdge(rhs, tv.id)
    // println("Adding matrix node to graph")
    graphDebug(dag.toGraphviz)
    // assert(dag.isCompletelyConnected, "Graph is not completely connected after adding matrix node")
  end matrixy

  inline def reductionWithParams[F[_], G[_]](
      tv: TejV[F, T],
      depId: UUID,
      op: ParameterisedReductionOps[InferDimension[F]],
      param: TupleDim[InferDimension[G]]
  )(using
      fs: Field[T],
      f: VectorisedField[F, T],
      g: VectorisedField[G, T],
      tr: VectorisedTrig[F, T],
      redF: Reductions[F, T, InferDimension[F]],
      redG: Reductions[G, T, InferDimension[G]],
      shF: Show[F[T]],
      shG: Show[G[T]],
      ct: ClassTag[T]
  ): Unit =
    val someG = dag.getNode(depId).asInstanceOf[VNode[G, T]].value
    val node = ReductionWithParams[F, G, T](op, tv.value, tv.id, depId, param, someG)
    dag.addNode(node)
    dag.addEdge(depId, tv.id)
    // assert(dag.isCompletelyConnected, "Graph is not completely connected after adding reduction with params node")
  end reductionWithParams

  // For reduction to scalars.
  inline def rowReduction(
      tv: TejV[Array, T],
      depId: UUID,
      op: ReductionOps,
      rowGrads: Array[T]
  )(using
      gfa: VectorisedField[Array, T],
      tr: VectorisedTrig[Array, T],
      f2: Field[T],
      shF: Show[Array[T]],
      shG: Show[Matrix[T]],
      ct: ClassTag[T]
  ): Unit =
    val node = RowReductionNode(op, tv.value, tv.id, depId, rowGrads)
    dag.addNode(node)
    dag.addEdge(depId, tv.id)
    // assert(dag.isCompletelyConnected, "Graph is not completely connected after adding row reduction node")
  end rowReduction

  def normaliseRows(dep: UUID, value: TejV[Matrix, T], op: NormaliseRowOps)(using
      f: Field[T],
      gfm: VectorisedField[Matrix, T],
      gfa: VectorisedField[Array, T],
      gta: VectorisedTrig[Matrix, T],
      red: Reductions[Array, T, 1],
      sh: Show[Matrix[T]],
      ct: ClassTag[T],
      nr: NRoot[T],
      n: Numeric[T],
      mty: Matrixy[Matrix, T]
  ): Unit =
    // println("adding normaliseRows node")
    val node = NormaliseRowsNode[T](value.value, value.id, dep, op)
    dag.addNode(node)
    dag.addEdge(dep, value.id)
    // assert(dag.isCompletelyConnected, "Graph is not completely connected after adding normaliseRows node")
    // TejV(newMat)
  end normaliseRows

  // This can't work.
  // inline def mapRowsNode(
  //     in: TejV[Matrix, T],
  //     out: TejV[Matrix, T],
  //     rows: Seq[(UUID, UUID)],
  // )(using
  //     gfa: VectorisedField[Matrix, T],
  //     tr: VectorisedTrig[Matrix, T],
  //     f2: Field[T],
  //     shG: Show[Matrix[T]],
  //     ct: ClassTag[T]
  // ): Unit =
  //   ???

  def selectIndicies(
      tv: TejV[Matrix, T],
      indicies: NArray[(Int, Int)],
      incomingId: UUID
  )(using
      f: VectorisedField[Matrix, T],
      m: Matrixy[Matrix, T],
      tr: VectorisedTrig[Matrix, T],
      sh: Show[Matrix[T]],
      ct: ClassTag[T]
  ): Unit =
    val node = SelectIndicesNode(tv.value, tv.id, incomingId, indicies)
    dag.addNode(node)
    dag.addEdge(incomingId, tv.id)

  end selectIndicies

  // follow torch nomenclature
  def arrange(
      tv: TejV[Array, T],
      indicies: NArray[(Int, Int)],
      incomingId: UUID
  )(using
      fa: VectorisedField[Array, T],
      t: VectorisedTrig[Array, T],
      fi: Field[T],
      sh: Show[Array[T]],
      m: Matrixy[Matrix, T],
      ct: ClassTag[T]
  ): Unit =
    val node = ArrangeNode(tv.value, tv.id, incomingId, indicies)
    dag.addNode(node)
    dag.addEdge(incomingId, tv.id)

    // assert(dag.isCompletelyConnected, "Graph is not completely connected after adding arrange node")

  end arrange

end TejVGraph
