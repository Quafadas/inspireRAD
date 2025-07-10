import io.github.quafadas.table.*

// import viz.PlotNt.plot
import org.apache.commons.math3.distribution.EnumeratedIntegerDistribution
import io.github.quafadas.spireAD.*

// import scala.NamedTuple
// import scala.NamedTuple.*
// import vecxt.all.*
// import viz.NamedTupleReadWriter.given
import vecxt.arrays.*
import viz.PlotTargets.doNothing
import viz.FromResource
import viz.PlotTarget
// import vecxt.*
import viz.PlotTargets
import pprint.*
import scala.collection.View
import scala.collection.immutable.HashMap
import vecxt.BoundsCheck.DoBoundsCheck.yes
import scala.util.chaining.*

import io.github.quafadas.spireAD.*
import spire.*
import spire.implicits.*
import spire.algebra.*
import vecxt.matrix.Matrix
import vecxt.all.fromRows
import vecxt.all.fromRowsArray
import vecxt.all.printArr
import vecxt.all.printMat
import vecxt.all.apply
import scala.reflect.ClassTag
import vecxt.all.row
import cats.syntax.all.toShow

type JsonMod = ujson.Value => Unit
case class HeatmapBigram(override val mods: Seq[JsonMod] = List())(using
    PlotTarget
) extends FromResource:
  override lazy val path = "HeatmapBigram.vg.json"

def heatmap(data: collection.Seq[(String, Int)])(using PlotTarget) =
  HeatmapBigram(
    Seq(
      (spec: ujson.Value) =>
        spec("data")("values") = data.map((a, b) =>
          ujson.Obj(
            "first" -> a.charAt(0).toString(),
            "second" -> a.charAt(1).toString(),
            "value" -> b
          )
        ),
      viz.Utils.fillDiv
    )
  )

def onehot(char: Char, allChars: collection.Map[Char, Int]): Array[Double] =
  val idx2 = allChars(char)
  Array.fill(allChars.size)(0.0).tap(_(idx2) = 1.0)

// @main def checkOneHot =
//   val chars = ('a' to 'z').toVector
//   val allChars = chars.zipWithIndex.toMap
//   val char = 'c'
//   val onehotChar = onehot(char, allChars)
//   println(onehotChar.printArr)

import cats.Show

// given Show[Matrix[Double]] with
//   def show(matrix: Matrix[Double]): String =
//     val rows =
//       for i <- 0 until matrix.rows
//       yield matrix
//         .row(i)
//         .map(s => "%.3f".format(s).reverse.padTo(6, ' ').reverse)
//         .mkString(" | ")
//     val footer = ("-" * (rows.head.length))
//     (rows :+ footer).mkString("\n")

// given Show[Array[Double]] with
//   def show(arr: Array[Double]): String =
//     arr.mkString("[", ", ", "]")

// given Show[Scalar[Double]] with
//   def show(arr: Scalar[Double]): String =
//     arr.scalar.toString

@main def makemore: Unit =

  val normalDist =
    new org.apache.commons.math3.distribution.NormalDistribution()

  val smooth = true

  val chars = '.' +: ('a' to 'z').toVector
  val charsIdx = chars.zipWithIndex.toMap

  def data: CsvIterator[Tuple1["name"]] = CSV.resource("names.txt")

  /** Bookended : adds "."to either end of the string Pairs : extracts the
    * series of pairs of the bookended characters characters Ints : indexes the
    * bookended characters xenc : one hot encodes the characters excluding the
    * last character
    */
  def bookended = data
    .addColumn["Bookend", String](s => s".${s.name}.")
    .addColumn["Pairs", Seq[String]](s => s.Bookend.sliding(2).toSeq)
    .addColumn["Ints", Seq[Int]](s => s.Bookend.map(charsIdx.seq))
    .addColumn["xenc", Seq[Array[Double]]](s =>
      s.Ints.init.map(i => onehot(chars(i), charsIdx.seq))
    )
    .addColumn["yenc", Array[Int]](s => s.Ints.tail.toArray)

  val combinations = for {
    a <- chars
    b <- chars
  } yield s"$a$b"

  println("Change lange to neural network")

  val dimensions = 27 * 27
  given td: TejDim[Double] = TejDim[Double]()
  val randoms = Array.fill(dimensions)(normalDist.sample())

  val W = Matrix(randoms, (27, 27))

  val xnencM =
    bookended
      // .take(useFirstn)
      .flatMap(_.xenc)

  val xencMall: Matrix[Double] = Matrix.fromRowsArray(
    xnencM.toArray
  )

  println(xencMall.shape)
  val yChars =
    bookended.flatMap(_.yenc).toArray

  inline def calcLoss[T](
      weights: Matrix[T],
      incomingData: Matrix[T],
      targets: Array[Int]
  )(using
      inline mOps: Matrixy[Matrix, T],
      inline fm: VectorisedField[Matrix, T],
      inline fa: VectorisedTrig[Array, T],
      inline fas: VectorisedField[Scalar, T],
      inline faa: VectorisedField[Array, T],
      inline redArr: Reductions[Array, T, 1],
      inline redMat: Reductions[Matrix, T, 2],
      inline t: VectorisedTrig[Matrix, T],
      nt: Numeric[T],
      ct: ClassTag[T]
  ): Scalar[T] =
    val logits = incomingData @@ weights
    val counts = logits.exp
    val probsNN = counts.mapRows(row => row / row.sum)
    val range = (0 until targets.length).toArray.zip(targets)
    -Scalar(probsNN(range).mapRowsToScalar(_.sum).log.mean)

  inline def calcLossF[T](
      weights: TejV[Matrix, T],
      incomingData: TejV[Matrix, T],
      targets: Array[Int]
  )(using
      mOps: Matrixy[Matrix, T],
      fm: VectorisedField[Matrix, T],
      fa: VectorisedField[Array, T],
      fas: VectorisedField[Scalar, T],
      fi: Field[T],
      redMat: Reductions[Matrix, T, 2],
      redArr: Reductions[Array, T, 1],
      vtm: VectorisedTrig[Matrix, T],
      vta: VectorisedTrig[Array, T],
      vts: VectorisedTrig[Scalar, T],
      dag: TejVGraph[T],
      nt: Numeric[T],
      ct: ClassTag[T],
      sh: Show[Matrix[T]],
      sha: Show[Array[T]],
      shs: Show[Scalar[T]]
  ): TejV[Scalar, T] =

    val logits = incomingData @@ weights
    val counts = logits.exp

    val probsNN = counts.normaliseRows
    val range: Array[(Int, Int)] = (0.until( targets.length)).toArray.zip(targets)
    val nearly = probsNN(range).mapRowsToScalar(ReductionOps.Sum).log.mean
    nearly * TejV(Scalar(fi.fromDouble(-1.0)))

  // import io.github.quafadas.spireAD.VectorisedField.elementwiseMatrixDoubleField
  // import io.github.quafadas.spireAD.VectorisedField.elementwiseArrayDoubleField
  // import io.github.quafadas.spireAD.Matrixy.matOps
  // import io.github.quafadas.spireAD.Reductions.vta
  // import io.github.quafadas.spireAD.Reductions.vtm
  // import io.github.quafadas.spireAD.VectorisedTrig.vtm

  val loss = calcLoss(W, xencMall, yChars)
  // println(loss.shape)

  import io.github.quafadas.spireAD.Matrixy.matOps
  given graph: TejVGraph[Double] = TejVGraph[Double]()

  // val lossF = calcLossF(W, xencMall, yChars)
  val lossT = TejV(W)
  println("lossT shape: " + lossT.value.shape)
  val menc = TejV(xencMall)
  val lossF = calcLossF[Double](lossT, menc, yChars)


  val grad = lossF.backward2((weights = lossT))

  println(grad.weights.shape)

  // println(grad)
  // println(expLossT.value.show)

  // println(out.head.grad.show)
  // println(out.last.grad)

  // println(graph.dag.toGraphviz)
  // println(lossT.tejNum(0, ::).printMat)
  // println(expLossT.tejNum(0, ::).printMat)
  // println(loss)

  // log of the "counts" of the pairs
  // val logits = xencM @@ W
  // val counts = logits.exp
  // val probsNN = counts.mapRows(row => row / row.sum)

  // val range = (0 to 5).toArray

  // val loss = probsNN(range, yChars).log.mean * -1.0

  // println(loss)
  // println(range)

  // println(probsNN.shape)

  // println(probsNN.row(2).sum)

  // println(probsNN.printMat)

  // println(loss)
