package io.github.quafadas.spireAD

import scala.math.*
import scala.reflect.*

import spire.algebra.*
import spire.syntax.isReal.*
import spire.syntax.vectorSpace.*
import scala.util.chaining.*
import scala.specialized as sp
import java.util.UUID
import cats.Show
import vecxt.matrix.Matrix
import narr.*

type NumDim[F[_]] <: Int =
  F[?] match
    case Array[?]  => 1
    case Matrix[?] => 2
    case _         => 0

case class TejVGraph[T: ClassTag]():

  final val dag = DAGV[T, VDimChangeNode[?, ?, T]]()

  inline def addToGraph[F[_]](t: TejV[F, T])(using
      vf: VectorisedField[F, T],
      vt: VectorisedTrig[F, T],
      sh: Show[F[T]]
  ): Unit =
    val n = VConstNode(t.value, t.id)
    dag.addNode(n)
  end addToGraph

  inline def reduction[F[_]](tv: TejV[F, T], op: ReductionOps, depId: UUID)(using
      f: VectorisedField[F, T],
      tr: VectorisedTrig[F, T],
      sh: Show[F[T]],
      ct: ClassTag[T]
  ): Unit =
    val node = ReductionNode[F, T](tv.value, tv.id, depId, op)
    dag.addNode(node)
    dag.addEdge(depId, tv.id)
  end reduction

  inline def unary[F[_]](tv: TejV[F, T], op: UrnaryOps, depId: UUID)(using
      f: VectorisedField[F, T],
      tr: VectorisedTrig[F, T],
      sh: Show[F[T]]
  ): Unit =
    val node = UrnaryNode[F, T](op, tv.value, tv.id, depId)
    dag.addNode(node)
    dag.addEdge(depId, tv.id)
  end unary

  inline def binary[F[_]](
      lhs: UUID,
      rhs: UUID,
      tv: TejV[F, T],
      op: BinaryOps
  )(using
      vf: VectorisedField[F, T],
      tr: VectorisedTrig[F, T],
      sh: Show[F[T]]
  ): Unit =
    val node = BinaryNode[F, T](op, tv.value, tv.id, lhs, rhs)
    dag.addNode(node)
    dag.addEdge(lhs, tv.id)
    dag.addEdge(rhs, tv.id)
  end binary

  inline def matrixy(
      lhs: UUID,
      rhs: UUID,
      tv: TejV[Matrix, T],
      op: MatrixyBinaryOps
  )(using
      vf: VectorisedField[Matrix, T],
      tr: VectorisedTrig[Matrix, T],
      mty: Matrixy[Matrix, T],
      sh: Show[Matrix[T]]
  ): Unit =
    val node = MatrixyNode[T](op, tv.value, tv.id, lhs, rhs)
    dag.addNode(node)
    dag.addEdge(lhs, tv.id)
    dag.addEdge(rhs, tv.id)
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
  end reductionWithParams

end TejVGraph

object TejV extends TejInstances:

  def apply[F[_], T](
      t: F[T]
  )(using td: TejVGraph[T], f: VectorisedField[F, T], tr: VectorisedTrig[F, T], sh: Show[F[T]]): TejV[F, T] =
    new TejV(value = t).tap(td.addToGraph)
  end apply

  private def createDontAddToGraph[F[_], T](
      t: F[T]
  )(using
      f: VectorisedField[F, T],
      tr: VectorisedTrig[F, T],
      sh: Show[F[T]]
  ): TejV[F, T] =
    new TejV(t)

  implicit def autoTejV[F[_], T](anF: F[T])(using
      c: ClassTag[T],
      td: TejVGraph[T],
      f: VectorisedField[F, T],
      sh: Show[F[T]],
      t: VectorisedTrig[F, T]
  ): TejV[F, T] =
    new TejV(value = anF).tap(td.addToGraph)

end TejV

@SerialVersionUID(0L)
final case class TejV[F[_], @sp(Float, Double) T] private (value: F[T])(using
    f: VectorisedField[F, T],
    sh: Show[F[T]]
) extends Serializable:
  lhs =>
  lazy val id = UUID.randomUUID()

  def exp(using t: VectorisedTrig[F, T], td: TejVGraph[T]) =
    new TejV(lhs.value.exp).tap(td.unary(_, UrnaryOps.Exp, lhs.id))
  end exp

  def log(using t: VectorisedTrig[F, T], td: TejVGraph[T]) =
    new TejV(lhs.value.log).tap(td.unary(_, UrnaryOps.Log, lhs.id))

  def sin(using t: VectorisedTrig[F, T], td: TejVGraph[T]) =
    new TejV(lhs.value.sin).tap(td.unary(_, UrnaryOps.Sin, lhs.id))

  def cos(using t: VectorisedTrig[F, T], td: TejVGraph[T]) =
    new TejV(lhs.value.cos).tap(td.unary(_, UrnaryOps.Cos, lhs.id))

  def +(rhs: TejV[F, T])(using f: VectorisedField[F, T], t: VectorisedTrig[F, T], td: TejVGraph[T], sh: Show[F[T]]) =
    new TejV(lhs.value + rhs.value).tap(td.binary(lhs.id, rhs.id, _, BinaryOps.Add))
  end +

  def -(rhs: TejV[F, T])(using f: VectorisedField[F, T], t: VectorisedTrig[F, T], td: TejVGraph[T], sh: Show[F[T]]) =
    new TejV(lhs.value - rhs.value).tap(td.binary(lhs.id, rhs.id, _, BinaryOps.Sub))
  end -

  def *(rhs: TejV[F, T])(using f: VectorisedField[F, T], t: VectorisedTrig[F, T], td: TejVGraph[T], sh: Show[F[T]]) =
    new TejV(lhs.value * rhs.value).tap(td.binary(lhs.id, rhs.id, _, BinaryOps.Mul))
  end *

  def /(rhs: TejV[F, T])(using f: VectorisedField[F, T], t: VectorisedTrig[F, T], td: TejVGraph[T], sh: Show[F[T]]) =
    new TejV(lhs.value / rhs.value).tap(td.binary(lhs.id, rhs.id, _, BinaryOps.Div))
  end /

  def sum(using
      td: TejVGraph[T],
      rd: Reductions[F, T, NumDim[F]],
      r: Numeric[T],
      vfScalar: VectorisedField[Scalar, T],
      vfTrig: VectorisedTrig[Scalar, T],
      sh: Show[Scalar[T]],
      ct: ClassTag[T]
  ): TejV[Scalar, T] =
    val newT = TejV.createDontAddToGraph(Scalar(value.sum))
    println(newT)
    newT.tap(td.reduction(_, ReductionOps.Sum, lhs.id))
  end sum

  def product(using
      td: TejVGraph[T],
      rd: Reductions[F, T, NumDim[F]],
      n: Numeric[T],
      vfScalar: VectorisedField[Scalar, T],
      sh: Show[Scalar[T]],
      vfTrig: VectorisedTrig[Scalar, T],
      ct: ClassTag[T]
  ): TejV[Scalar, T] =
    TejV.createDontAddToGraph(Scalar(value.product)).tap(td.reduction(_, ReductionOps.Product, lhs.id))
  end product

  def mean(using
      td: TejVGraph[T],
      rd: Reductions[F, T, NumDim[F]],
      n: Numeric[T],
      vfScalar: VectorisedField[Scalar, T],
      sh: Show[Scalar[T]],
      vfTrig: VectorisedTrig[Scalar, T],
      ct: ClassTag[T]
  ): TejV[Scalar, T] =
    TejV.createDontAddToGraph(Scalar(value.mean)).tap(td.reduction(_, ReductionOps.Mean, lhs.id))
  end mean

  def backward[G[_]](wrt: Set[TejV[G, T]])(using td: TejVGraph[T], ct: ClassTag[T]) =
    val graph = td.dag.toposort
    val reversed = graph.reverse

    reversed.head.setGradOne

    // This _may_ prevent a bunch of uncessary work. need to check.
    // val minIndex = reversed.zipWithIndex.collect {
    //   case (node, index) if wrt.exists(_.id == node.id) => index
    // }.min

    // println(s"minIndex: $minIndex")

    reversed.foreach { node =>
      node.backward
    }
    val ids = wrt.map(_.id)
    td.dag.getAllNodes.filter(n => ids.contains(n.id)).asInstanceOf[Set[VNode[G, T]]]
  end backward

  def @@(rhs: TejV[Matrix, T])(using
      f: VectorisedField[Matrix, T],
      t: VectorisedTrig[Matrix, T],
      td: TejVGraph[T],
      sh: Show[Matrix[T]],
      ev: F[T] <:< Matrix[T],
      matTc: Matrixy[Matrix, T]
  ): TejV[Matrix, T] =
    val newmat = matTc.@@(value)(rhs.value)
    new TejV(newmat).tap(mval => td.matrixy(lhs.id, rhs.id, mval, MatrixyBinaryOps.MatMul))
  end @@

  def mapRowsToScalar(fct: (NArray[T] => T))(using
      f: VectorisedField[NArray, T],
      t: VectorisedTrig[NArray, T],
      td: TejVGraph[T],
      shm: Show[Matrix[T]],
      sha: Show[NArray[T]],
      ev: F[T] <:< Matrix[T],
      matTc: Matrixy[Matrix, T]
  ): TejV[NArray, T] =
    val newmat = matTc.mapRowsToScalar(value)(fct)
    new TejV(newmat)
  end mapRowsToScalar

  def mapRows(fct: (NArray[T] => NArray[T]))(using
      f: VectorisedField[NArray, T],
      t: VectorisedTrig[NArray, T],
      vfm: VectorisedField[Matrix, T],
      td: TejVGraph[T],
      shm: Show[Matrix[T]],
      sha: Show[NArray[T]],
      ev: F[T] <:< Matrix[T],
      matTc: Matrixy[Matrix, T]
  ): TejV[Matrix, T] =
    val newmat = value.asInstanceOf[Matrix[T]].mapRows(fct)
    new TejV(newmat)
  end mapRows

  def apply(i: NArray[(Int, Int)])(using
      td: TejVGraph[T],
      rd: Reductions[Matrix, T, 2],
      vf: VectorisedField[Matrix, T],
      vt: VectorisedTrig[F, T],
      vfa: VectorisedField[NArray, T],
      ev: F[T] <:< Matrix[T],
      matTc: Matrixy[Matrix, T],
      ct: ClassTag[T],
      sh: Show[Matrix[T]],
      ord: Numeric[T]
  ): TejV[Matrix, T] =
    val newT = value.asInstanceOf[Matrix[T]]
    val newT2 = matTc.apply(newT)(i)
    new TejV[Matrix, T](newT)
  end apply

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
