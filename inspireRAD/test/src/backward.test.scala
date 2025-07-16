package io.github.quafadas.inspireRAD

import munit.FunSuite

import spire.*
import spire.math.*
import spire.implicits.*
import _root_.algebra.ring.Field
import spire.algebra.Trig
import scala.math.Ordering.Implicits.infixOrderingOps
import narr.*
import vecxt.all.*
import cats.syntax.all.toShow
import cats.Show

class BackwardSuite extends FunSuite:

  given Show[Matrix[Double]] with
    def show(matrix: Matrix[Double]): String =
      val rows =
        for i <- 0 until matrix.rows
        yield matrix
          .row(i)
          .map(s => "%.3f".format(s).reverse.padTo(6, ' ').reverse)
          .mkString(" | ")
      // val footer = ("-" * (rows.head.length))
      rows.mkString("\n")
    end show
  end given

  given Show[Array[Double]] with
    def show(arr: Array[Double]): String =
      arr.mkString("[", ", ", "]")
  end given

  given Show[Scalar[Double]] with
    def show(arr: Scalar[Double]): String =
      arr.scalar.toString
  end given

  // def jetIsNumericAndFractional[T: Field: Ring: ClassTag](using jd: JetDim) =
  //   new JetIsNumericAndFractional[Jet[T], T]:
  //     def plus(x: Jet[T], y: Jet[T]): Jet[T] = x + y
  //     def minus(x: Jet[T], y: Jet[T]): Jet[T] = x - y
  //     def times(x: Jet[T], y: Jet[T]): Jet[T] = x * y
  //     def negate(x: Jet[T]): Jet[T] = -x
  //     def fromInt(x: Int): Jet[T] = Jet(summon[Field[T]].fromInt(x))
  //     def toInt(x: Jet[T]): Int = x.toInt
  //     def toLong(x: Jet[T]): Long = x.toLong
  //     def toFloat(x: Jet[T]): Float = x.toFloat
  //     def toDouble(x: Jet[T]): Double = x.toDouble
  //     def compare(x: Jet[T], y: Jet[T]): Int = ??? // if x.real > y.real then 1 else if x.real < y.real then -1 else 0
  //     def div(x: Jet[T], y: Jet[T]): Jet[T] = x / y

  // trait JetIsNumericAndFractional[Jet, T] extends scala.Fractional[T] {}

  // trait TejIsNumericAndFractional[Tej, T] extends scala.Fractional[T] {}

  // def tejIsNumericAndFractional[T: Field: Ring: ClassTag](using td: TejDim[T]) =
  //   new TejIsNumericAndFractional[Tej[T], T]:
  //     def plus(x: Tej[T], y: Tej[T]): Tej[T] = x + y
  //     def minus(x: Tej[T], y: Tej[T]): Tej[T] = x - y
  //     def times(x: Tej[T], y: Tej[T]): Tej[T] = x * y
  //     def negate(x: Tej[T]): Tej[T] = -x
  //     def fromInt(x: Int): Tej[T] = Tej(summon[Field[T]].fromInt(x))
  //     def toInt(x: Tej[T]): Int = x.toInt
  //     def toLong(x: Tej[T]): Long = x.toLong
  //     def toFloat(x: Tej[T]): Float = x.toFloat
  //     def toDouble(x: Tej[T]): Double = x.toDouble
  //     def compare(x: Tej[T], y: Tej[T]): Int = ??? //  if x > y then 1 else if x < y then -1 else 0
  //     def div(x: Tej[T], y: Tej[T]): Tej[T] = x / y
  lazy given oJet: Ordering[Jet[Double]] = Ordering.by(_.real)
  lazy given oTTej: Ordering[Tej[Double]] = Ordering.by(_.value)

  // def writeGraph(using td: TejDim[Double]) =
  //   val graph = td.dag.toGraphviz
  //   os.write.over(os.Path("/Users/simon/Code/spire_AD/spireAD/experiments/tmp.dot"), graph)
  // end writeGraph

  test("completely connectedness") {
    import vecxt.BoundsCheck.DoBoundsCheck.no
    given graph: TejVGraph[Double] = TejVGraph[Double]()
    val x = TejV(Matrix.fromRows(NArray(1.0, 2.0, 3.0)))
    val y = TejV(Matrix.fromRows(NArray(4.0, 5.0, 6.0)))
    // graphDebug(graph.dag.toGraphviz)
    val z = x + y
    assert(graph.dag.isCompletelyConnected)
    // println(graph.dag.toGraphviz)
  }

  test(" throws if not completely connected") {
    import vecxt.BoundsCheck.DoBoundsCheck.no
    given graph: TejVGraph[Double] = TejVGraph[Double]()
    val x = TejV(Matrix.fromRows(NArray(1.0, 2.0, 3.0)))
    val y = TejV(Matrix.fromRows(NArray(4.0, 5.0, 6.0)))
    val z = x + y
    // This gets added to the graph, but does nothing! It might nerf the toposort...
    val oops = TejV(Matrix.fromRows(NArray(4.0, 5.0, 6.0)))

    assert(!graph.dag.isCompletelyConnected)
    // println(graph.dag.toGraphviz)

    intercept[java.lang.AssertionError] {
      z.backward((x = x, y = y, oops = oops))
    }
  }

  test("log softmax gradients sum to zero. Result are consistent between Tej, Jet, double") {
    val dim = 4
    given jetd: JetDim = JetDim(dim)

    def logSoftmax[T: Trig: ClassTag: Ordering](x: Array[T])(using f: Field[T]): Array[T] =
      val xmax = x.fold(f.zero)((a, b) => if a > b then a else b)
      val shifted = x.map(x => x - xmax) // Stabilization
      val logSumExp = spire.math.log(shifted.map(spire.math.exp).foldLeft(f.zero)(_ + _))
      shifted.map(x => x - logSumExp)
    end logSoftmax

    given jd: TejDim[Double] = TejDim()

    val range = (1 to dim).toArray.map(_.toDouble).tejArr
    val rangeJ = (1 to dim).toArray.map(_.toDouble).jetArr

    val res = logSoftmax(range.toArray.map(_.toDouble)).sum
    val logSoftmaxResult = logSoftmax(range).foldLeft(Tej(0.0))(_ + _)
    val logSoftmaxResultJ = logSoftmax(rangeJ).foldLeft(Jet(0.0))(_ + _)

    val out = logSoftmaxResult.backward(range)
    assertEqualsDouble(logSoftmaxResultJ.infinitesimal.sum, 0, 0.0001)
    assertEqualsDouble(out.map(_._2).sum, 0, 0.0001)

    assertEqualsDouble(res, logSoftmaxResultJ.real, 0.0001)
    assertEqualsDouble(res, logSoftmaxResult.value, 0.0001)
  }

  test("softmax gradients sum to zero. Result are consistent between Tej, Jet, double") {
    val dim = 4
    given jetd: JetDim = JetDim(dim)
    given jd: TejDim[Double] = TejDim()

    def softmax[T: Trig: ClassTag](x: Array[T])(using f: Field[T]): Array[T] =
      val expValues = x.map(spire.math.exp)
      val sumExpValues = expValues.foldLeft(f.zero)(_ + _)
      expValues.map(_ / sumExpValues)
    end softmax

    val range = (1 to dim).toArray.map(_.toDouble).tejArr
    val rangeJ = (1 to dim).toArray.map(_.toDouble).jetArr

    val res = softmax(range.toArray.map(_.toDouble)).sum
    val logSoftmaxResult = softmax(range).foldLeft(Tej(0.0))(_ + _)
    val logSoftmaxResultJ = softmax(rangeJ).foldLeft(Jet(0.0))(_ + _)

    val out = logSoftmaxResult.backward(range)

    assertEqualsDouble(res, 1.0, 0.000001)
    assertEqualsDouble(res, logSoftmaxResultJ.real, 0.0001)
    assertEqualsDouble(res, logSoftmaxResult.value, 0.0001)

  }

  test("Backpropagation for arrange selects correct gradients") {
    import vecxt.BoundsCheck.DoBoundsCheck.yes
    given graph: TejVGraph[Double] = TejVGraph[Double]()
    val n = 5
    val arangeArray = NArray.tabulate(n)(i => i.toDouble)
    val x = Matrix(arangeArray, rows = n, cols = 1)
    val xVar = TejV(x)

    // Select elements at rows 0, 3, 4 (col is always 0)
    val selected = xVar.arrange(Array((0, 0), (3, 0), (4, 0)))

    // f = sum of squares of selected elements
    val f = (selected * selected).sum

    // Backprop gradients to xVar
    val grads = f.backward((x = xVar))
    val gradMatrix = grads.x

    val gradRaw = gradMatrix.raw.toArray

    // Expected: grad = 2 * x[i] at selected positions, zero elsewhere
    val expectedGrad = Array.fill(n)(0.0)
    val selectedIndices = Array(0, 3, 4)
    selectedIndices.foreach(i => expectedGrad(i) = 2.0 * arangeArray(i))

    // Assert gradient correctness
    for i <- 0 until n do
      assert(
        math.abs(gradRaw(i) - expectedGrad(i)) < 1e-8,
        s"Grad at index $i incorrect: found ${gradRaw(i)}, expected ${expectedGrad(i)}"
      )
    end for
  }

  test("Tiny regression") {
    import vecxt.BoundsCheck.DoBoundsCheck.no
    val X = Matrix.fromRows(
      NArray(-2.0, 4),
      NArray(-1.0, 1),
      NArray(0.0, 0),
      NArray(1.0, 1),
      NArray(2.0, 4)
    )

    val y = Matrix(NArray(4.0, 1.0, 0.0, 1.0, 4.0), rows = 5, cols = 1)

    val weightsInit = Matrix.fromColumns(Array(1.0, 1.0))
    def loss(
        data: TejV[Matrix, Double],
        weights: TejV[Matrix, Double],
        targets: TejV[Matrix, Double]
    )(using
        mOps: Matrixy[Matrix, Double],
        fm: VectorisedField[Matrix, Double],
        fa: VectorisedField[Array, Double],
        fas: VectorisedField[Scalar, Double],
        fi: Field[Double],
        redMat: Reductions[Matrix, Double, 2],
        redArr: Reductions[Array, Double, 1],
        redSca: Reductions[Scalar, Double, 0],
        vtm: VectorisedTrig[Matrix, Double],
        vta: VectorisedTrig[Array, Double],
        vts: VectorisedTrig[Scalar, Double],
        dag: TejVGraph[Double],
        nt: Numeric[Double],
        ct: ClassTag[Double],
        sh: Show[Matrix[Double]],
        sha: Show[Array[Double]],
        shs: Show[Scalar[Double]]
    ): TejV[Scalar, Double] =
      val preds = data @@ weights
      val errors = preds - targets
      (errors * errors).sum.div(TejV(Scalar(data.value.rows.toDouble)))
    end loss

    @annotation.tailrec
    def train(
        weights: Matrix[Double],
        data: Matrix[Double],
        targets: Matrix[Double],
        learningRate: Double,
        steps: Int
    ): TejV[Matrix, Double] =
      given graph: TejVGraph[Double] = TejVGraph[Double]()
      val weights_ = TejV(weights)
      val data_ = TejV(data)

      if steps == 0 then weights_
      else
        val lossCalculated = loss(data_, weights_, targets.tej)
        // graphDebug(graph.dag.toGraphviz)
        val grad = lossCalculated.backward((weights = weights_), false)

        val updated = weights_ - (grad.weights * learningRate).tej
        // println(s"Updated weights: ${updated.value.show}")
        // if` steps % 3 == 0 then
        //   `println(s"Step $steps, loss: ${lossCalculated.value.scalar}")
        if steps - 1 == 0 then println(s"Final loss: ${lossCalculated.value.scalar}")
        end if
        train(updated.value, data, targets, learningRate, steps - 1)
      end if
    end train

    val weights = train(weightsInit, X, y, 0.01, 100)
    // println(s"trained on 100 steps")
    // println(s"Final weights: ${weights.value.show}")

    assertEqualsDouble(weights.value(0, 0), 0.01, 0.01)
    assertEqualsDouble(weights.value(1, 0), 1.0, 0.01)
  }
  // test("A tiny example") {
  //   val X = Matrix.fromRows(
  //       NArray(0, 0),
  //       NArray(0, 1),
  //       NArray(1, 0),
  //       NArray(1, 1)
  //   )

  //   val y = Matrix.fromRows(
  //       NArray(0),
  //       NArray(1),
  //       NArray(1),
  //       NArray(0)
  //   )

  //   // Simple neural network: 2-2-1, tanh hidden, sigmoid output, binary cross-entropy loss
  //   import scala.util.Random

  //   val inputSize = 2
  //   val hiddenSize = 2
  //   val outputSize = 1
  //   val lr = 0.1
  //   val epochs = 1000

  //   val rand = new Random(42)
  //   def randn(rows: Int, cols: Int) = Matrix.tabulate(rows, cols)((_, _) => rand.nextGaussian())

  //   var W1 = randn(inputSize, hiddenSize)
  //   var b1 = Matrix.zeros(1, hiddenSize)
  //   var W2 = randn(hiddenSize, outputSize)
  //   var b2 = Matrix.zeros(1, outputSize)

  //   def sigmoid(x: Matrix): Matrix = x.map(v => 1.0 / (1.0 + math.exp(-v)))
  //   def sigmoidDeriv(x: Matrix): Matrix = {
  //     val s = sigmoid(x)
  //     s * (Matrix.ones(s.rows, s.cols) - s)
  //   }
  //   def tanh(x: Matrix): Matrix = x.map(math.tanh)
  //   def tanhDeriv(x: Matrix): Matrix = x.map(v => 1 - math.tanh(v) * math.tanh(v))

  //   def binaryCrossEntropy(yHat: Matrix, y: Matrix): Double =
  //     val eps = 1e-8
  //     val yClipped = yHat.map(v => math.max(eps, math.min(1 - eps, v)))
  //     val loss = -(y * yClipped.map(math.log) + (Matrix.ones(y.rows, y.cols) - y) * (Matrix.ones(y.rows, y.cols) - yClipped).map(math.log))
  //     loss.data.sum / y.rows

  //   for epoch <- 1 to epochs do
  //     // Forward
  //     val z1 = X * W1 + b1
  //     val a1 = tanh(z1)
  //     val z2 = a1 * W2 + b2
  //     val a2 = sigmoid(z2)

  //     // Loss
  //     val loss = binaryCrossEntropy(a2, y)

  //     // Backward
  //     val dz2 = a2 - y
  //     val dW2 = a1.T * dz2 / X.rows
  //     val db2 = dz2.sumRows / X.rows

  //     val da1 = dz2 * W2.T
  //     val dz1 = da1 * tanhDeriv(z1)
  //     val dW1 = X.T * dz1 / X.rows
  //     val db1 = dz1.sumRows / X.rows

  //     // Update
  //     W1 = W1 - dW1 * lr
  //     b1 = b1 - db1 * lr
  //     W2 = W2 - dW2 * lr
  //     b2 = b2 - db2 * lr

  //     if epoch % 200 == 0 then
  //     println(s"Epoch $epoch, loss: $loss")
  //   end for

  //   // Prediction
  //   val preds = sigmoid(tanh(X * W1 + b1) * W2 + b2)
  //   println("Predictions:")
  //   println(preds)

  // }

end BackwardSuite
