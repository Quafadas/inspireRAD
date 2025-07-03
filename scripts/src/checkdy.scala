import io.github.quafadas.spireAD.*
import vecxt.all.*
import scala.reflect.ClassTag
import vecxt.BoundsCheck.DoBoundsCheck.yes
import cats.syntax.all.toShow
import cats.Show

import spire.*
import spire.math.JetDim
import spire.implicits.*
import _root_.algebra.ring.Field
import spire.compat.numeric
import spire.math.Jet

given Show[Matrix[Double]] with
  def show(matrix: Matrix[Double]): String =
    val rows =
      for i <- 0 until matrix.rows
      yield matrix
        .row(i)
        .map(s => "%.3f".format(s).reverse.padTo(6, ' ').reverse)
        .mkString(" | ")
    val footer = ("-" * (rows.head.length))
    (rows :+ footer).mkString("\n")
  end show
end given

given jd2: Show[Matrix[Jet[Double]]] = new Show[Matrix[Jet[Double]]]:
  def show(matrix: Matrix[Jet[Double]]): String =
    val rows =
      for i <- 0 until matrix.rows
      yield matrix
        .row(i)
        .map(s => s.toString().reverse.padTo(10, ' ').reverse)
        .mkString(" | ")
    val footer = ("-" * (rows.head.length))
    (rows :+ footer).mkString("\n")
  end show

given Show[Array[Double]] with
  def show(arr: Array[Double]): String =
    arr.mkString("[", ", ", "]")
end given

given Show[Scalar[Double]] with
  def show(arr: Scalar[Double]): String =
    arr.scalar.toString
end given

@main def checkefy =
  inline def calcLoss[@specialized(Double) T](
      weights: Matrix[T],
      incomingData: Matrix[T],
      targets: Array[Int]
  )(using
      inline mOps: Matrixy[Matrix, T],
      inline fas: VectorisedField[Scalar, T],
      inline faa: VectorisedField[Array, T],
      inline fm: VectorisedField[Matrix, T],
      inline fa: VectorisedTrig[Array, T],
      inline t: VectorisedTrig[Matrix, T],
      inline redArr: Reductions[Array, T, 1],
      inline redMat: Reductions[Matrix, T, 2],
      inline showable: Show[Matrix[T]],
      nt: Numeric[T],
      ct: ClassTag[T]
  ): Scalar[T] =
    val logits = incomingData @@ weights
    // println(logits.show)
    val counts = logits.exp
    val probsNN = counts.mapRows(row => row / row.sum)
    val range = (0 until targets.length).toArray.zip(targets)
    val ranged = mOps.apply(probsNN)(range)
    -Scalar(ranged.mapRowsToScalar(_.sum).log.mean)
  end calcLoss

  val mat1 = Matrix.fromRows(
    Array(1.0, 2.0, 3.0, 4.0),
    Array(1.0, 2.0, 3.0, 4.0),
    Array(1.0, 2.0, 3.0, 4.0),
    Array(1.0, 2.0, 3.0, 4.0)
  )

  val mat2 = Matrix.fromRows(
      Array(1.0, 2.0, 3.0, 4.0) / 10
    )

  val targets = Array(1, 0)

  val calc1 = calcLoss(
    mat1,
    mat2,
    targets
  )

  println("calc1:")
  println(calc1.show)
  println("-----")



  {
    given jd: JetDim = JetDim(16)
    import VectorisedField.jetNumeric

    given vf: VectorisedField[Array, Jet[Double]] = VectorisedField.elementwiseArrayJetField
    given vm: VectorisedField[Matrix, Jet[Double]] = VectorisedField.elementwiseMatrixJetField
    given vs: VectorisedField[Scalar, Jet[Double]] = VectorisedField.scalarJetField
    given rja: Reductions[Array, Jet[Double], 1] = Reductions.vtaJet
    given rjm: Reductions[Matrix, Jet[Double], 2] = Reductions.vtmJet

    given doubleJM: Matrixy[Matrix, Jet[Double]] = Matrixy.doubleJetIsMatrixy(using yes, jd)

    val calcJet = calcLoss(
      Matrix.fromRows(
        Array(1.0, 2.0, 3.0, 4.0).jetArr,
        Array(1.0, 2.0, 3.0, 4.0).jetArr(4),
        Array(1.0, 2.0, 3.0, 4.0).jetArr(8),
        Array(1.0, 2.0, 3.0, 4.0).jetArr(12)
      ),
      Matrix.fromRows(
        (Array(1.0, 2.0, 3.0, 4.0) / 10).jetArr
      ),
      Array(1, 0)
    )

    println("jet Calc:")
    println(calcJet)
    println("-----")
  }

  given tvg : TejVGraph[Double] = TejVGraph[Double]()

  println("instantiate")

  val tv1 = TejV(mat1)
  val tv2 = TejV(mat2)

  val logits = tv2 @@ tv1
  println("logits")
  val counts = logits.exp
  println("counts")
  val probsNN = counts.mapRows{row =>
    val s = row.sum
    row.div(s.value)
  }
  println("probsNN")
  val range = (0 until targets.length).toArray.zip(targets)
  println("range")
  val ranged = probsNN(range)
  println("ranged")
  val loss = ranged.mapRowsToScalar(ReductionOps.Sum)
  println("mapped to scalars")
  loss.log.mean

  println("loss")
  // // now the Tej
  // given tejDim: TejDim[Double] = TejDim()

  // given tej_N: Numeric[Tej[Double]] = VectorisedField.tejNumeric
  // given tej_mOps: Matrixy[Matrix, Tej[Double]] = Matrixy.doubleTejIsMatrixy(using yes, tejDim)
  // given tej_scalar: VectorisedField[Scalar, Tej[Double]] = VectorisedField.scalarTejField
  // given tej_array: VectorisedField[Array, Tej[Double]] = VectorisedField.elementwiseArrayTejField


  // val calcTej = calcLoss(
  //   Matrix.fromRows(
  //     Array(1.0, 2.0, 3.0, 4.0).tejArr,
  //     Array(1.0, 2.0, 3.0, 4.0).tejArr,
  //     Array(1.0, 2.0, 3.0, 4.0).tejArr,
  //     Array(1.0, 2.0, 3.0, 4.0).tejArr
  //   ),
  //   Matrix.fromRows(
  //     (Array(1.0, 2.0, 3.0, 4.0) / 10).tejArr
  //   ),
  //   Array(1, 0)


end checkefy
