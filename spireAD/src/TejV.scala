package io.github.quafadas.spireAD

import scala.math.*
import scala.reflect.*
import scala.NamedTuple.*
import spire.algebra.*
import scala.util.chaining.*
import scala.specialized as sp
import java.util.UUID
import cats.Show
import vecxt.matrix.Matrix
import narr.*
import vecxt.BoundsCheck

// Type-level function to extract gradient types from TejV types
type GradientTypes[V <: Tuple] <: Tuple = V match
  case EmptyTuple         => EmptyTuple
  case TejV[f, t] *: tail => f[t] *: GradientTypes[tail]

extension (d: Double) def tej(using td: TejVGraph[Double], sh: Show[Scalar[Double]]) = TejV(Scalar(d))
end extension

extension (m: Array[Double])
  def tej(using td: TejVGraph[Double], sh: Show[Array[Double]], f: VectorisedField[Array, Double]) =
    TejV(m)
end extension

extension (m: Matrix[Double])
  def tej(using
      td: TejVGraph[Double],
      sh: Show[Matrix[Double]],
      f: VectorisedField[Matrix, Double],
      tr: VectorisedTrig[Matrix, Double],
      ev: Matrixy[Matrix, Double]
  ) =
    TejV(m)
end extension

object TejV extends TejInstances:

  def apply[F[_], T](
      t: F[T]
  )(using td: TejVGraph[T], f: VectorisedField[F, T], tr: VectorisedTrig[F, T], sh: Show[F[T]]): TejV[F, T] =
    new TejV(value = t).tap(td.addToGraph)
  end apply

  def createDontAddToGraph[F[_], T](
      t: F[T]
  )(using
      f: VectorisedField[F, T],
      tr: VectorisedTrig[F, T],
      sh: Show[F[T]]
  ): TejV[F, T] =
    new TejV(t)

  // implicit def autoTejV[F[_], T](anF: F[T])(using
  //     c: ClassTag[T],
  //     td: TejVGraph[T],
  //     f: VectorisedField[F, T],
  //     sh: Show[F[T]],
  //     t: VectorisedTrig[F, T]
  // ): TejV[F, T] =
  //   new TejV(value = anF).tap(td.addToGraph)

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

  def +(
      rhs: TejV[F, T]
  )(using f: VectorisedField[F, T], t: VectorisedTrig[F, T], td: TejVGraph[T], fi: Field[T], sh: Show[F[T]]) =
    new TejV(lhs.value + rhs.value).tap(td.binary(lhs.id, rhs.id, _, BinaryOps.Add))
  end +

  def +(rhs: TejV[Scalar, T])(using
      f: VectorisedField[F, T],
      fs: VectorisedField[Scalar, T],
      t: VectorisedTrig[F, T],
      ts: VectorisedTrig[Scalar, T],
      td: TejVGraph[T],
      fi: Field[T],
      sh: Show[F[T]],
      shs: Show[Scalar[T]],
      rd: Reductions[F, T, InferDimension[F]],
      n: Numeric[T],
      ct: ClassTag[T]
  ): TejV[F, T] =
    // val fid = f.fromDouble(d)
    val newVal = f.+(lhs.value)(rhs.value.scalar)
    new TejV(newVal).tap(td.scalar[F](lhs.id, rhs.id, _, BinaryScalarOps.Add, rhs.value.scalar))
  end +

  def -(
      rhs: TejV[F, T]
  )(using f: VectorisedField[F, T], t: VectorisedTrig[F, T], td: TejVGraph[T], fi: Field[T], sh: Show[F[T]]) =
    new TejV(lhs.value - rhs.value).tap(td.binary(lhs.id, rhs.id, _, BinaryOps.Sub))
  end -

  def *(
      rhs: TejV[F, T]
  )(using f: VectorisedField[F, T], t: VectorisedTrig[F, T], td: TejVGraph[T], fi: Field[T], sh: Show[F[T]]) =
    new TejV(lhs.value * rhs.value).tap(td.binary(lhs.id, rhs.id, _, BinaryOps.Mul))
  end *

  def /(
      rhs: TejV[F, T]
  )(using
      f: VectorisedField[F, T],
      t: VectorisedTrig[F, T],
      td: TejVGraph[T],
      sh: Show[F[T]],
      fi: Field[T]
  ): TejV[F, T] =
    new TejV(lhs.value / rhs.value).tap(td.binary(lhs.id, rhs.id, _, BinaryOps.Div))
  end /

  def div(rhs: TejV[Scalar, T])(using
      f: VectorisedField[F, T],
      t: VectorisedTrig[F, T],
      td: TejVGraph[T],
      sh: Show[F[T]],
      fi: Field[T],
      ct: ClassTag[T],
      n: Numeric[T],
      red: Reductions[F, T, InferDimension[F]]
  ): TejV[F, T] =
    val newVal = f./(lhs.value)(rhs.value.scalar)
    new TejV(newVal).tap(
      td.scalar(lhs.id, rhs.id, _, BinaryScalarOps.Div, rhs.value.scalar)
    )
  end div

  def clampMin(threshold: T)(using
      f: VectorisedField[F, T],
      t: VectorisedTrig[F, T],
      td: TejVGraph[T],
      sh: Show[F[T]],
      fi: Field[T],
      ct: ClassTag[T],
      red: Reductions[F, T, InferDimension[F]],
      n: Numeric[T]
  ): TejV[F, T] =
    val newVal = f.clampMin(lhs.value)(threshold)
    new TejV(newVal).tap(td.scalar(lhs.id, lhs.id, _, BinaryScalarOps.ClampMin, threshold))
  end clampMin

  def sum(using
      td: TejVGraph[T],
      rd: Reductions[F, T, NumDim[F]],
      r: Numeric[T],
      vfScalar: VectorisedField[Scalar, T],
      vfTrig: VectorisedTrig[Scalar, T],
      f: Field[T],
      sh: Show[Scalar[T]],
      ct: ClassTag[T]
  ): TejV[Scalar, T] =
    val newT = TejV.createDontAddToGraph(Scalar(value.sum))
    // println(newT)
    newT.tap(td.reduction(_, lhs, ReductionOps.Sum, lhs.id))
  end sum

  def normaliseRowsL1(using
      td: TejVGraph[T],
      vf: VectorisedField[Array, T],
      vf2: VectorisedField[Matrix, T],
      vt2: VectorisedTrig[Matrix, T],
      red: Reductions[Array, T, 1],
      n: Numeric[T],
      f: Field[T],
      sh: Show[Matrix[T]],
      mty: Matrixy[Matrix, T],
      nr: NRoot[T],
      ct: ClassTag[T],
      ev: F[T] <:< Matrix[T]
  ): TejV[Matrix, T] =
    val newMat = mty.mapRows(value) { (row: Array[T]) =>
      val su = row.sum
      vf./(row)(su)
    }

    new TejV(newMat).tap(
      td.normaliseRows(this.id, _, NormaliseRowOps.NormaliseRowsL1)
    )
  end normaliseRowsL1

  def normaliseRowsL2(using
      td: TejVGraph[T],
      vf: VectorisedField[Array, T],
      vf2: VectorisedField[Matrix, T],
      vt2: VectorisedTrig[Matrix, T],
      red: Reductions[Array, T, 1],
      n: Numeric[T],
      f: Field[T],
      sh: Show[Matrix[T]],
      mty: Matrixy[Matrix, T],
      nr: NRoot[T],
      ct: ClassTag[T],
      ev: F[T] <:< Matrix[T]
  ): TejV[Matrix, T] =
    val newMat = mty.mapRows(value) { (row: Array[T]) =>
      val l2 = nr.sqrt(row.map(x => n.times(x, x)).sum)
      vf./(row)(l2)
    }

    new TejV(newMat).tap(
      td.normaliseRows(this.id, _, NormaliseRowOps.NormaliseRowsL2)
    )
  end normaliseRowsL2

  def softmaxRows(using
      td: TejVGraph[T],
      vf: VectorisedField[Array, T],
      vf2: VectorisedField[Matrix, T],
      vt2: VectorisedTrig[Matrix, T],
      vta: VectorisedTrig[Array, T],
      red: Reductions[Array, T, 1],
      n: Numeric[T],
      f: Field[T],
      sh: Show[Matrix[T]],
      nr: NRoot[T],
      mty: Matrixy[Matrix, T],
      ct: ClassTag[T],
      ev: F[T] <:< Matrix[T]
  ): TejV[Matrix, T] =
    val newMat = mty.mapRows(value) { (row: Array[T]) =>

      val norm = vf.-(row)(row.max)
      val expd = vta.exp(norm)
      val summed = red.sum(expd)
      vf./(expd)(summed)
    }

    new TejV(newMat).tap(
      td.normaliseRows(this.id, _, NormaliseRowOps.Softmax)
    )
  end softmaxRows

  def product(using
      td: TejVGraph[T],
      rd: Reductions[F, T, NumDim[F]],
      n: Numeric[T],
      vfScalar: VectorisedField[Scalar, T],
      sh: Show[Scalar[T]],
      vfTrig: VectorisedTrig[Scalar, T],
      f: Field[T],
      ct: ClassTag[T]
  ): TejV[Scalar, T] =
    TejV.createDontAddToGraph(Scalar(value.product)).tap(td.reduction(_, lhs, ReductionOps.Product, lhs.id))
  end product

  def mean(using
      td: TejVGraph[T],
      rd: Reductions[F, T, NumDim[F]],
      n: Numeric[T],
      vfScalar: VectorisedField[Scalar, T],
      sh: Show[Scalar[T]],
      vfTrig: VectorisedTrig[Scalar, T],
      f: Field[T],
      ct: ClassTag[T]
  ): TejV[Scalar, T] =
    TejV.createDontAddToGraph(Scalar(value.mean)).tap(td.reduction(_, lhs, ReductionOps.Mean, lhs.id))
  end mean

  // def backward[G[_]](wrt: Set[TejV[?, T]], debug: Boolean = false)(using
  //     td: TejVGraph[T],
  //     ct: ClassTag[T]
  // ): Set[VNode[G, T]] =
  //   if debug || !td.dag.isCompletelyConnected then os.write.over(os.pwd / "graph.dot", td.dag.toGraphviz)
  //   end if
  //   assert(td.dag.isCompletelyConnected, "Graph is not completely connected before backward pass.")
  //   val graph = td.dag.toposort
  //   val reversed = graph.reverse

  //   reversed.head.setGradOne

  //   // This _may_ prevent a bunch of uncessary work. need to check.
  //   // val minIndex = reversed.zipWithIndex.collect {
  //   //   case (node, index) if wrt.exists(_.id == node.id) => index
  //   // }.min

  //   // println(s"minIndex: $minIndex")

  //   if debug then println("---> Backward pass for TejV")
  //   end if

  //   reversed.foreach { node =>
  //     if debug then println("node: " + node.graphShow)
  //     end if
  //     node.backward
  //   }
  //   val ids = wrt.map(_.id)
  //   td.dag.getAllNodes.filter(n => ids.contains(n.id)).asInstanceOf[Set[VNode[G, T]]]
  // end backward

  // Simpler approach: return a regular Tuple with type info preserved at call site
  def backward[N <: Tuple, V <: Tuple](wrt: NamedTuple[N, V], debug: Boolean = false)(using
      td: TejVGraph[T],
      ct: ClassTag[T]
  ) =
    if debug || !td.dag.isCompletelyConnected then os.write.over(os.pwd / "graph.dot", td.dag.toGraphviz)
    end if
    assert(td.dag.isCompletelyConnected, "Graph is not completely connected before backward pass.")
    val graph = td.dag.toposort
    val reversed = graph.reverse

    reversed.head.setGradOne

    if debug then println("---> Backward pass for TejV")
    end if

    reversed.foreach { node =>
      if debug then println("node: " + node.graphShow)
      end if

      node.backward
    }

    // Extract gradients while preserving tuple structure
    def extractGradientTuple(t: Tuple): Tuple = t match
      case EmptyTuple => EmptyTuple
      case h *: tail  =>
        val tejv = h.asInstanceOf[TejV[?, T]]
        val grad = td.dag.getNode(tejv.id).grad
        grad *: extractGradientTuple(tail)

    val gradientTuple = extractGradientTuple(wrt.toTuple)
    if debug then
      println(" Graph after backward pass:--------")
      println(td.dag.toGraphviz)
    end if
    if debug then println("-----------Backward pass complete")
    end if

    // Return with names preserved - the caller will have the correct types
    gradientTuple.asInstanceOf[NamedTuple[N, GradientTypes[V]]]
  end backward

  def @@(rhs: TejV[Matrix, T])(using
      f: VectorisedField[Matrix, T],
      f2: VectorisedField[Array, T],
      t: VectorisedTrig[Matrix, T],
      td: TejVGraph[T],
      sh: Show[Matrix[T]],
      ev: F[T] <:< Matrix[T],
      fi: Field[T],
      matTc: Matrixy[Matrix, T]
  ): TejV[Matrix, T] =
    val newmat = matTc.@@(value)(rhs.value)
    new TejV(newmat).tap(mval => td.matrixy(lhs.id, rhs.id, mval, MatrixyBinaryOps.MatMul))
  end @@

  def mapRowsToScalar(fct: ReductionOps)(using
      f: VectorisedField[NArray, T],
      t: VectorisedTrig[NArray, T],
      rd: Reductions[NArray, T, 1],
      td: TejVGraph[T],
      f2: Field[T],
      shm: Show[Matrix[T]],
      sha: Show[NArray[T]],
      ev: F[T] <:< Matrix[T],
      matTc: Matrixy[Matrix, T],
      ct: ClassTag[T]
  ): TejV[NArray, T] =
    val rowfct = fct match
      case ReductionOps.Sum =>
        ((row: NArray[T]) => row.sum)
      case ReductionOps.Product =>
        ((row: NArray[T]) => row.product)
      case ReductionOps.Mean =>
        ((row: NArray[T]) => row.mean)

    val newmat = matTc.mapRowsToScalar(value)(rowfct)

    // println(td.dag.toGraphviz)
    new TejV(newmat).tap(newVal =>
      // td.addToGraph(newVal)
      td.rowReduction(
        newVal,
        lhs.id,
        fct,
        newVal.value
      )
    )

  end mapRowsToScalar

  // def mapRows(fct: TejV[Array, T] => TejV[Array, T])(using
  //     f: VectorisedField[NArray, T],
  //     t: VectorisedTrig[Matrix, T],
  //     tA: VectorisedTrig[Array, T],
  //     vfm: VectorisedField[Matrix, T],
  //     td: TejVGraph[T],
  //     fi: Field[T],
  //     shm: Show[Matrix[T]],
  //     sha: Show[NArray[T]],
  //     ev: F[T] <:< Matrix[T],
  //     matTc: Matrixy[Matrix, T],
  //     ct:ClassTag[T]
  // ): TejV[Matrix, T] =

  //   val mat = value.asInstanceOf[Matrix[T]]
  //   val listIds = scala.collection.mutable.ListBuffer.empty[(UUID, UUID)]
  //   val newmat = mat.mapRows(row =>
  //     val oldRow = TejV(row)(using td, f, tA, sha)
  //     val rowV = fct(oldRow)
  //     listIds.addOne(
  //       (oldRow.id, rowV.id)
  //     )
  //     rowV.value
  //   )

  //   val newTej = TejV.createDontAddToGraph(newmat)

  //   td.mapRowsNode(
  //     in = this.asInstanceOf[TejV[Matrix, T]],
  //     out = newTej,
  //     rows = listIds.toSeq
  //   )

  //   newTej

  // end mapRows

  def apply(i: NArray[(Int, Int)])(using
      td: TejVGraph[T],
      rd: Reductions[Matrix, T, 2],
      vf: VectorisedField[Matrix, T],
      vt: VectorisedTrig[Matrix, T],
      vfa: VectorisedField[NArray, T],
      ev: F[T] <:< Matrix[T],
      matTc: Matrixy[Matrix, T],
      ct: ClassTag[T],
      sh: Show[Matrix[T]],
      ord: Numeric[T]
  ): TejV[Matrix, T] =
    val newT = value.asInstanceOf[Matrix[T]]
    val newT2 = matTc.apply(newT)(i)
    new TejV[Matrix, T](newT2).tap(tv => td.selectIndicies(tv, i, lhs.id))
  end apply

  def arrange(i: NArray[(Int, Int)])(using
      td: TejVGraph[T],
      fi: Field[T],
      vf: VectorisedField[Matrix, T],
      vt: VectorisedTrig[Array, T],
      vfa: VectorisedField[NArray, T],
      ev: F[T] <:< Matrix[T],
      matTc: Matrixy[Matrix, T],
      ct: ClassTag[T],
      ord: Numeric[T],
      sh: Show[Array[T]]
  ): TejV[Array, T] =
    import vecxt.BoundsCheck.DoBoundsCheck.no
    val newT = value.asInstanceOf[Matrix[T]]

    val newT2 = matTc.arrange(newT)(i)
    new TejV[Array, T](newT2).tap(tv => td.arrange(tv, i, lhs.id))
  end arrange

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
