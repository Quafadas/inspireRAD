package io.github.quafadas.spireAD

import munit.*

import spire.*
import spire.math.*
import spire.implicits.DoubleAlgebra
import cats.Show
import spire.implicits.ArrayNormedVectorSpace
import narr.*
import vecxt.MatrixHelper.fromRows
import vecxt.matrix.Matrix
import vecxt.all.mapRowsToScalar
import vecxt.all.printMat
import vecxt.all.printArr
import cats.syntax.show.toShow
import vecxt.all.row
import vecxt.MatrixInstance.apply
import vecxt.matrix.Matrix.apply
import vecxt.all.mapRows

class DAGVSuite extends FunSuite:

  import VectorisedTrig.vta
  import VectorisedField.elementwiseArrayDoubleField
  import Reductions.vtm

  given Show[Matrix[Double]] with
    def show(arr: Matrix[Double]): String = vecxt.all.printMat(arr)
  end given

  given smj: Show[Matrix[Jet[Double]]] = new Show[Matrix[Jet[Double]]]:
    def show(arr: Matrix[Jet[Double]]): String =
      val rows =
        for i <- 0 until arr.rows
        yield arr
          .row(i)
          .map(s => s.toString().reverse.padTo(10, ' ').reverse)
          .mkString(" | ")
      val footer = ("-" * (rows.head.length))
      (rows :+ footer).mkString("\n")
    end show

  given Show[Array[Double]] with
    def show(arr: Array[Double]): String = arr.mkString("[", ", ", "]")
  end given

  given Show[Scalar[Double]] with
    def show(arr: Scalar[Double]): String = arr.scalar.toString
  end given

  test("f(x) = exp(sin(x))") {

    val arr = Array(1.0, 2.0, 3.0)
    given jd: JetDim = JetDim(3)
    val jetArr = arr.jetArr.map(sin).map(exp)

    // println(jetArr.mkString("\n"))

    given tejV: TejVGraph[Double] = TejVGraph[Double]()

    val tej = TejV(arr)

    val fct = tej.sin.exp

    for i <- 0 until arr.length do
      assertEquals(
        fct.value(i),
        jetArr(i).real
      )
    end for

    val out: Set[VNode[Array, Double]] = fct.backward(Set(tej))

    assert(out.size == 1)
    // println(out.head.grad.mkString("\n"))

    for i <- 0 until arr.length do
      assertEquals(
        out.head.grad(i),
        jetArr(i).infinitesimal(i)
      )
    end for

  }

  test("TejV +") {

    val (arr1, arr2, jetArrPlus) = prepareJetArrays(_ + _)
    // println(jetArrPlus.mkString("\n"))

    given tejV: TejVGraph[Double] = TejVGraph[Double]()

    val tej1 = TejV(arr1)
    val tej2 = TejV(arr2)

    val tejPlus = tej1 + tej2

    for i <- 0 until arr1.length do
      assertEquals(
        tejPlus.value(i),
        jetArrPlus(i).real
      )
    end for

    val out: Set[VNode[Array, Double]] = tejPlus.backward[Array](Set(tej1, tej2))

    assert(out.size == 2)
    val grad: Array[Double] = out.head.grad + out.last.grad

    for i <- 0 until arr1.length do
      assertEquals(
        grad(i),
        jetArrPlus(i).infinitesimal(i)
      )
    end for
  }
  test("TejV -") {

    val (arr1, arr2, jetArrPlus) = prepareJetArrays(_ - _)

    given tejV: TejVGraph[Double] = TejVGraph[Double]()

    val tej1 = TejV(arr1)
    val tej2 = TejV(arr2)

    val tejPlus = tej1 - tej2

    for i <- 0 until arr1.length do
      assertEquals(
        tejPlus.value(i),
        jetArrPlus(i).real
      )
    end for

    val out = tejPlus.backward[Array](Set(tej1, tej2))

    assert(out.size == 2)
    val grad = out.head.grad + out.last.grad

    for i <- 0 until arr1.length do
      assertEquals(
        grad(i),
        jetArrPlus(i).infinitesimal(i)
      )
    end for
  }
  test("TejV *") {

    val (arr1, arr2, jetArrPlus) = prepareJetArrays(_ * _)

    // println(jetArrPlus.mkString("\n"))

    given tejV: TejVGraph[Double] = TejVGraph[Double]()

    val tej1 = TejV(arr1)
    val tej2 = TejV(arr2)

    val tejPlus = tej1 * tej2

    for i <- 0 until arr1.length do
      assertEquals(
        tejPlus.value(i),
        jetArrPlus(i).real
      )
    end for

    val out = tejPlus.backward[Array](Set(tej1, tej2))

    assert(out.size == 2)
    val grad = out.head.grad + out.last.grad

    for i <- 0 until arr1.length do
      assertEquals(
        grad(i),
        jetArrPlus(i).infinitesimal(i)
      )
    end for
  }

  def prepareJetArrays(
      op: (Jet[Double], Jet[Double]) => Jet[Double]
  ): (Array[Double], Array[Double], Array[Jet[Double]]) =
    given jd: JetDim = JetDim(3)
    val arr1 = Array(1.0, 2.0, 3.0)
    val arr2 = Array(4.0, 5.0, 8.0)

    val jetArr1 = arr1.jetArr
    val jetArr2 = arr2.jetArr

    val jetArrPlus = jetArr1.zip(jetArr2).map((a, b) => op(a, b))
    (arr1, arr2, jetArrPlus)
  end prepareJetArrays

  test("TejV /") {

    val (arr1, arr2, jetArrPlus) = prepareJetArrays(_ / _)

    // println(jetArrPlus.mkString("\n"))

    given tejV: TejVGraph[Double] = TejVGraph[Double]()
    val tej1 = TejV(arr1)
    val tej2 = TejV(arr2)

    val tejPlus = tej1 / tej2

    for i <- 0 until arr1.length do
      assertEqualsDouble(
        tejPlus.value(i),
        jetArrPlus(i).real,
        0.0000001
      )
    end for

    val out = tejPlus.backward[Array](Set(tej1, tej2))

    assert(out.size == 2)
    val grad = out.head.grad + out.last.grad

    for i <- 0 until arr1.length do
      assertEqualsDouble(
        grad(i),
        jetArrPlus(i).infinitesimal(i),
        0.0000001
      )
    end for
  }

  test("Adding the same container without the Tej wqrapper") {
    given tejV: TejVGraph[Double] = TejVGraph[Double]()
    val arr = Array(1.0, 2.0, 3.0, 4.0)
    val tej = TejV(arr)

    val t2 = tej + Array(1.0, 2.0, 3.0, 4.0)
    // TODO ???
    // val t3 = arr + tej
    assert(tejV.dag.toposort.size == 3)
    assertEquals(t2.value.toSeq, Seq(2.0, 4.0, 6.0, 8.0))
  }

  test("Reduction operations - sum") {
    given tejV: TejVGraph[Double] = TejVGraph[Double]()
    val arr = Array(1.0, 2.0, 3.0, 4.0)
    val tej = TejV(arr)

    val summed = tej.sum
    // val product = tej.product
    // val mean = tej.mean

    assertEquals(summed.value.scalar, 10.0)
    // assertEquals(product.value, 24.0)
    // assertEquals(mean.value, 2.5)

    given jd: JetDim = JetDim(4)
    val jetArr = arr.jetArr

    val jetCalc = jetArr.foldLeft(Jet(0.0))((acc, j) => acc + j)

    val out = summed.backward[Array](Set(tej))
    val grad = out.tail.foldLeft(out.head.grad)((acc, j) => acc + j.grad)

    for i <- 0 until jetArr.length do
      assertEqualsDouble(
        grad(i),
        jetCalc.infinitesimal(i),
        0.0000001
      )
    end for

  }
  test("Reduction operations - product") {
    given tejV: TejVGraph[Double] = TejVGraph[Double]()
    val arr = Array(1.0, 2.0, 3.0, 4.0)
    val tej = TejV(arr)

    val summed = tej.product
    assertEqualsDouble(summed.value.scalar, 24.0, 0.0000001)

    given jd: JetDim = JetDim(4)
    val jetArr = arr.jetArr

    val jetCalc = jetArr.foldLeft(Jet(1.0))((acc, j) => acc * j)

    val out = summed.backward[Array](Set(tej))
    val grad = out.tail.foldLeft(out.head.grad)((acc, j) => acc + j.grad)

    for i <- 0 until jetArr.length do
      assertEqualsDouble(
        grad(i),
        jetCalc.infinitesimal(i),
        0.0000001
      )
    end for

  }

  test("reduction operations - mean") {
    given tejV: TejVGraph[Double] = TejVGraph[Double]()
    val arr = Array(1.0, 2.0, 3.0, 4.0)
    val tej = TejV(arr)

    val summed = tej.mean
    assertEqualsDouble(summed.value.scalar, 2.5, 0.0000001)

    given jd: JetDim = JetDim(4)
    val jetArr = arr.jetArr

    val jetCalc = jetArr.foldLeft(Jet(0.0))((acc, j) => acc + j) / arr.length

    val out = summed.backward[Array](Set(tej))
    val grad = out.tail.foldLeft(out.head.grad)((acc, j) => acc + j.grad)
    for i <- 0 until jetArr.length do
      assertEqualsDouble(
        grad(i),
        jetCalc.infinitesimal(i),
        0.0000001
      )
    end for

  }

  test("Row reductions - sum") {
    import vecxt.BoundsCheck.DoBoundsCheck.yes
    given tejV: TejVGraph[Double] = TejVGraph[Double]()
    val arr = vecxt.all.Matrix.fromRows(
      Array(1.0, 2.0, 3.0),
      Array(5.0, 6.0, 7.0),
      Array(9.0, 10.0, 11.0)
    )
    val tej = TejV(arr)

    val summed = tej.mapRowsToScalar(ReductionOps.Sum)
    assertEqualsDouble(summed.value(0), 6.0, 0.000001)
    assertEqualsDouble(summed.value(1), 18.0, 0.000001)
    assertEqualsDouble(summed.value(2), 30.0, 0.000001)

    // given jd: JetDim = JetDim(9)
    // val jetArr = arr.raw.jetArr

    // val jetMat = Matrix[Jet[Double]](
    //   arr.shape,
    //   jetArr
    // )

    // val jetCalc = jetMat.mapRowsToScalar(
    //   row => row.foldLeft(Jet(0.0))((acc, j) => acc + j)
    // )

    val out = summed.backward[Matrix](Set(tej))

    val tejGrad = out.head.grad

    for i <- 0 until tejGrad.raw.length do
      assertEqualsDouble(
        tejGrad.raw(i),
        1.0,
        0.0000001
      )
    end for

  }

  test("Row reductions - mean") {
    import vecxt.BoundsCheck.DoBoundsCheck.yes
    given tejV: TejVGraph[Double] = TejVGraph[Double]()
    val arr = vecxt.all.Matrix.fromRows(
      Array(1.0, 2.0, 3.0),
      Array(5.0, 6.0, 7.0),
      Array(9.0, 10.0, 11.0)
    )
    val tej = TejV(arr)

    val summed = tej.mapRowsToScalar(ReductionOps.Mean)
    assertEqualsDouble(summed.value(0), 2.0, 0.000001)
    assertEqualsDouble(summed.value(1), 6.0, 0.000001)
    assertEqualsDouble(summed.value(2), 10.0, 0.000001)

    // given jd: JetDim = JetDim(9)
    // val jetArr = arr.raw.jetArr

    // val jetMat = Matrix[Jet[Double]](
    //   arr.shape,
    //   jetArr
    // )

    // val jetCalc = jetMat.mapRowsToScalar(
    //   row => row.foldLeft(Jet(0.0))((acc, j) => acc + j)
    // )

    val out = summed.backward[Matrix](Set(tej))

    val tejGrad = out.head.grad

    for i <- 0 until tejGrad.raw.length do
      assertEqualsDouble(
        tejGrad.raw(i),
        0.33333333,
        0.0000001
      )
    end for

  }

  test("Scalar division") {

    given td: TejVGraph[Double] = TejVGraph[Double]()

    val j = TejV(Scalar(2.0))
    val arr = TejV(Array(1.0, 2.0, 3.0, 4.0))

    val res = arr.div(j)

    assertEqualsDouble(res.value(0), 0.5, 0.000001)
    assertEqualsDouble(res.value(1), 1.0, 0.000001)
    assertEqualsDouble(res.value(2), 1.5, 0.000001)
    assertEqualsDouble(res.value(3), 2.0, 0.000001)

    val out = res.backward[Array](Set(arr))

    val tejGrad = out.head.grad

    for i <- 0 until tejGrad.length do
      assertEqualsDouble(
        tejGrad(i),
        0.5,
        0.0000001
      )
    end for

    td.dag.resetGrads

    val gradJ = res.backward[Scalar](Set(j)).head.grad
    assertEqualsDouble(gradJ.scalar, -2.5, 0.000001)
  }

  test("Scalar clampMin") {
    import vecxt.BoundsCheck.DoBoundsCheck.yes
    given td: TejVGraph[Double] = TejVGraph[Double]()

    val arr =
      Matrix.fromRows(
        Array(-1.0, 2.0, -9.0, 4.0),
        Array(1.5, -2.0, 3.0, -4.0)
      )

    val tej = TejV(arr)
    val res = tej.clampMin(0.0)

    val resM = Matrix(
      arr.raw.clampMin(0.0),
      arr.shape
    )
    val grad = Matrix.fromRows(
      Array(0.0, 1.0, 0.0, 1.0),
      Array(1.0, 0.0, 1.0, 0.0)
    )

    val out = res.backward[Matrix](Set(tej))

    val tejGrad = out.head.grad

    for i <- 0 until arr.rows do
      for j <- 0 until arr.cols do
        assertEqualsDouble(
          res.value(i, j),
          resM(i, j),
          0.0000001
        )
        assertEqualsDouble(
          tejGrad(i, j),
          grad(i, j),
          0.0000001
        )

      end for
    end for
  }

  test("Row reductions - product") {
    import vecxt.BoundsCheck.DoBoundsCheck.yes
    given tejV: TejVGraph[Double] = TejVGraph[Double]()
    val arr = vecxt.all.Matrix.fromRows(
      Array(1.0, 2.0, 3.0),
      Array(5.0, 6.0, 7.0),
      Array(9.0, 10.0, 11.0)
    )
    val tej = TejV(arr)

    val summed = tej.mapRowsToScalar(ReductionOps.Product)
    assertEqualsDouble(summed.value(0), 6.0, 0.000001)
    assertEqualsDouble(summed.value(1), 210.0, 0.000001)
    assertEqualsDouble(summed.value(2), 990.0, 0.000001)

    given jd: JetDim = JetDim(9)
    val jetArr = arr.raw.jetArr

    val jetMat = Matrix[Jet[Double]](
      arr.shape,
      jetArr
    )

    val jetCalc = jetMat.mapRowsToScalar(row => row.foldLeft(Jet(1.0))((acc, j) => acc * j))

    // println(jetCalc.show)

    val out = summed.backward[Matrix](Set(tej))

    val tejGrad = out.head.grad

    // println(tejGrad.show)

    assertEqualsDouble(
      tejGrad.raw(0),
      6.0,
      0.0000001
    )

    assertEqualsDouble(
      tejGrad.raw.last,
      90.0,
      0.0000001
    )

  }
  test("Row reductions - normalise") {
    import vecxt.BoundsCheck.DoBoundsCheck.yes
    given tejV: TejVGraph[Double] = TejVGraph[Double]()
    val arr = vecxt.all.Matrix.fromRows(
      Array(1.0, 2.0, 3.0)
    )

    val tej = TejV(arr).div(TejV(Scalar(10.0)))

    val summed = tej.normaliseRows
    val mat = summed.value
    assertEqualsDouble(mat(0, 0), 1.0 / 6.0, 0.000001)
    val out = summed.backward[Matrix](Set(tej))
    // os.write.over(os.Path("/Users/simon/Code/spire_AD/") / "graph.viz", tejV.dag.toGraphviz)

    val tejGrad = out.head.grad
    assertEqualsDouble(
      tejGrad.raw(0),
      0.83333333,
      0.0000001
    )

    assertEqualsDouble(
      tejGrad.raw(1),
      0.6666666,
      0.0000001
    )

    assertEqualsDouble(
      tejGrad.raw(2),
      0.5,
      0.0000001
    )

  }

  test("Row reductions - softmax") {
    import vecxt.BoundsCheck.DoBoundsCheck.yes
    given tejV: TejVGraph[Double] = TejVGraph[Double]()
    val arr = vecxt.all.Matrix.fromRows(
      Array(1.0, 2.0, 3.0)
    )

    val tej = TejV(arr)

    val softmaxed = tej.softmaxRows
    val mat = softmaxed.value

    val max = arr.raw.max
    val expRow = arr.raw.map(x => math.exp(x - max))
    val sumExp = expRow.sum
    val expected = expRow.map(_ / sumExp)

    for i <- 0 until arr.raw.length do assertEqualsDouble(mat(0, i), expected(i), 0.000001)
    end for

    val out = softmaxed.backward[Matrix](Set(tej))
    val tejGrad = out.head.grad

    // Compute expected gradient for softmax
    // inomcing gradients are all 1.0

    for i <- 0 until arr.raw.length do
      val dot = expected.sum
      val grad = expected(i) * (1 - dot)

      assertEqualsDouble(tejGrad.raw(i), grad, 0.000001)
    end for

  }

  test("Multiply two scalars") {
    given tejV: TejVGraph[Double] = TejVGraph[Double]()
    val a = TejV(Scalar(2.0))
    val b = TejV(Scalar(3.0))

    val res = a * b

    assertEqualsDouble(res.value.scalar, 6.0, 0.000001)

    val out = res.backward[Scalar](Set(a, b))

    assertEqualsDouble(out.head.grad.scalar, 3.0, 0.000001)
    assertEqualsDouble(out.last.grad.scalar, 2.0, 0.000001)
  }

  // test("backward 2") {

  // }

  // I don't think this can ever work.
  // Because if you do something like normalisation - e.g. row /  row.sum , the row.sum _is not an indepant constant_. Normalisation needs the jacobian gradient.
  // test("map rows".only) {
  //   import vecxt.BoundsCheck.DoBoundsCheck.yes
  //   given tejV: TejVGraph[Double] = TejVGraph[Double]()
  //   val arr = vecxt.all.Matrix.fromRows(
  //     Array(1.0, 2.0, 3.0),
  //     Array(5.0, 6.0, 7.0),
  //     Array(9.0, 10.0, 11.0)
  //   )
  //   val tej = TejV(arr)

  //   val normalised = tej.mapRows(row => row.div(row.sum) )
  //   val out = normalised.sum
  //   val mat = normalised.value
  //   assertEqualsDouble(mat.raw(0), 0.16666667, 0.000001)

  //   given jd: JetDim = JetDim(9)
  //   val jetArr = arr.raw.jetArr

  //   val jetMat = Matrix[Jet[Double]](
  //     arr.shape,
  //     jetArr
  //   )

  //   val jetCalc = jetMat.mapRows{row =>
  //     row.map(j => j / row.foldLeft(Jet(0.0))((acc, j) => acc + j))
  //   }

  //   val backward = out.backward(Set(tej))

  //   println(os.pwd)
  //   os.write.over(os.Path("/Users/simon/Code/spire_AD/spireAD/") / "graph.viz", tejV.dag.toGraphviz)
  //   println(mat.printMat)

  //   println(jetCalc.show)

  //   val tejGrad = backward.head.grad

  //   // println(tejGrad.show)

  //   for i <- 0 until tejGrad.raw.length do
  //     assertEqualsDouble(
  //       tejGrad.raw(i),
  //       2.0,
  //       0.0000001
  //     )
  //   end for

  // }

  test("select elements") {
    import vecxt.BoundsCheck.DoBoundsCheck.yes
    given tejV: TejVGraph[Double] = TejVGraph[Double]()
    val mat = Matrix.fromRows(
      Array(1.0, 2.0, 3.0, 4.0),
      Array(1.0, 2.0, 3.0, 4.0) + 4.0
    )

    val tej = TejV(mat)

    val indices = NArray((0, 0), (1, 1), (0, 1), (1, 3))

    val matGrad = Matrix.fromRows(
      Array(1.0, 1.0, 0, 0),
      Array(0, 1.0, 0, 1.0)
    )

    val graph = tej(indices)

    val gradBack = graph.backward[Matrix](Set(tej))
    val gradCalculated = gradBack.head.grad

    // println(gradCalculated.printMat)

    for i <- 0 until mat.shape(0) do
      for j <- 0 until mat.shape(1) do
        assertEqualsDouble(
          gradCalculated(i, j),
          matGrad(i, j),
          0.0000001
        )
      end for
    end for

  }

end DAGVSuite
