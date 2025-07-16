package io.github.quafadas.inspireRAD

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

def graphDebug(s: String) =
  os.write.over(os.Path("/Users/simon/Code/spire_AD/") / "graph.dot", s)

class DAGVSuite extends FunSuite:

  import VectorisedTrig.vta
  import VectorisedField.elementwiseArrayDoubleField
  import Reductions.redMat

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

    val grads = fct.backward((tej = tej))

    assert(grads.size == 1)
    // println(out.head.grad.mkString("\n"))

    for i <- 0 until arr.length do
      assertEquals(
        grads.tej(i),
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

    val grads = tejPlus.backward((tej1 = tej1, tej2 = tej2))

    assert(grads.size == 2)
    val grad: Array[Double] = grads.tej1 + grads.tej2

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

    val out = tejPlus.backward((tej1 = tej1, tej2 = tej2))

    assert(out.size == 2)
    val grad = out.tej1 + out.tej2

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

    val out = tejPlus.backward((tej1 = tej1, tej2 = tej2))

    assert(out.size == 2)
    val grad = out.tej1 + out.tej2

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

    val out = tejPlus.backward((tej1 = tej1, tej2 = tej2))

    assert(out.size == 2)
    val grad = out.tej1 + out.tej2

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

    val t2 = tej + Array(1.0, 2.0, 3.0, 4.0).tej
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

    val out = summed.backward((tej = tej))
    // val grad = out.tail.foldLeft(out.head.grad)((acc, j) => acc + j.grad)

    for i <- 0 until jetArr.length do
      assertEqualsDouble(
        out.tej(i),
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

    val out = summed.backward((tej = tej))
    // val grad = out.tej.tail.foldLeft(out.tej.head)((acc, j) => acc + j.grad)

    for i <- 0 until jetArr.length do
      assertEqualsDouble(
        out.tej(i),
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

    val out = summed.backward((tej = tej))

    for i <- 0 until jetArr.length do
      assertEqualsDouble(
        out.tej(i),
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

    val grad = summed.backward((tej = tej))

    for i <- 0 until grad.tej.raw.length do
      assertEqualsDouble(
        grad.tej.raw(i),
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

    val grad = summed.backward((tej = tej))

    for i <- 0 until grad.tej.raw.length do
      assertEqualsDouble(
        grad.tej.raw(i),
        0.33333333,
        0.0000001
      )
    end for

  }

  test("Scalar division") {

    given td: TejVGraph[Double] = TejVGraph[Double]()

    val c = TejV(Scalar(2.0))
    val x = TejV(Array(2.0, 4.0, 6.0))

    val res = x.div(c)

    assertEquals(td.dag.getAllNodes.size, 3)
    assertEquals(td.dag.getAllEdges.size, 2)

    assertEqualsDouble(res.value(0), 1.0, 0.000001)
    assertEqualsDouble(res.value(1), 2.0, 0.000001)
    assertEqualsDouble(res.value(2), 3, 0, 0.000001)

    val grads = res.backward((x = x, c = c))
    // graphDebug(td.dag.toGraphviz)

    for i <- 0 until grads.x.length do
      assertEqualsDouble(
        grads.x(i),
        0.5,
        0.0000001
      )
    end for

    // too hard for now
    // assertEqualsDouble(
    //   grads.c.scalar,
    //   -3.0,
    //   0.001
    // )

  }

  test("Scalar division mat") {

    import vecxt.BoundsCheck.DoBoundsCheck.yes

    given td: TejVGraph[Double] = TejVGraph[Double]()

    val c = TejV(Scalar(2.0))
    val x = TejV(
      Matrix.fromRows(
        Array(2.0, 4.0),
        Array(6.0, 8.0)
      )
    )

    val res = x.div(c)

    assertEquals(td.dag.getAllNodes.size, 3)
    assertEquals(td.dag.getAllEdges.size, 2)

    val grads = res.backward((x = x, c = c))
    // graphDebug(td.dag.toGraphviz)

    for i <- 0 until grads.x.raw.length do
      assertEqualsDouble(
        grads.x.raw(i),
        0.5,
        0.0000001
      )
    end for

    // assertEqualsDouble(
    //   grads.c.scalar,
    //   -5.0,
    //   0.001
    // )

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

    val out = res.backward((tej = tej))

    for i <- 0 until arr.rows do
      for j <- 0 until arr.cols do
        assertEqualsDouble(
          res.value(i, j),
          resM(i, j),
          0.0000001
        )
        assertEqualsDouble(
          out.tej(i, j),
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

    val grad = summed.backward((tej = tej))

    assertEqualsDouble(
      grad.tej.raw(0),
      6.0,
      0.0000001
    )

    assertEqualsDouble(
      grad.tej.raw.last,
      90.0,
      0.0000001
    )

  }
  test("Row reductions - normalise L1") {
    import vecxt.BoundsCheck.DoBoundsCheck.yes
    given graph: TejVGraph[Double] = TejVGraph[Double]()
    val arr = vecxt.all.Matrix.fromRows(
      Array(1.0, 2.0, 3.0)
    )

    val tej = TejV(arr)
    val divvd = tej.div(TejV(Scalar(10.0)))

    val summed = divvd.normaliseRowsL1.sum
    val mat = summed.value
    assertEqualsDouble(summed.value.scalar, 1.0, 0.000001)
    val grad = summed.backward((tej = tej))

    assertEquals(graph.dag.getAllNodes.size, 5)

    // graphDebug(graph.dag.toGraphviz)

    assertEqualsDouble(grad.tej.raw(0), 0.0833333, 0.0000001)
    assertEqualsDouble(grad.tej.raw(1), 0, 0.0000001)
    assertEqualsDouble(grad.tej.raw(2), -0.0833333333, 0.0000001)

  }

  test("Matrix sum Binary div") {

    import vecxt.BoundsCheck.DoBoundsCheck.yes
    given tejV: TejVGraph[Double] = TejVGraph[Double]()
    val arr = vecxt.all.Matrix.fromRows(
      Array(1.0, 2.0, 3.0)
    )
    val tej = TejV(arr)
    val scalar = TejV(Scalar(10.0))

    val calc = tej.sum.div(scalar)

    assertEqualsDouble(calc.value.scalar, 0.6, 0.000001)

    // graphDebug(tejV.dag.toGraphviz)
    val grad = calc.backward((tej = tej, scalar = scalar))
    // graphDebug(tejV.dag.toGraphviz)

    grad.tej.raw.foreach { x =>
      assertEqualsDouble(x, 0.1, 0.000001)
    }

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

    val calcGrad = softmaxed.backward((tej = tej))

    // Compute expected gradient for softmax
    // inomcing gradients are all 1.0

    for i <- 0 until arr.raw.length do
      val dot = expected.sum
      val grad = expected(i) * (1 - dot)

      assertEqualsDouble(calcGrad.tej.raw(i), grad, 0.000001)
    end for

  }

  test("Multiply two scalars") {
    given tejV: TejVGraph[Double] = TejVGraph[Double]()
    val a = TejV(Scalar(2.0))
    val b = TejV(Scalar(3.0))

    val res = a * b

    assertEqualsDouble(res.value.scalar, 6.0, 0.000001)

    val out = res.backward((a = a, b = b))

    assertEqualsDouble(out.a.scalar, 3.0, 0.000001)
    assertEqualsDouble(out.b.scalar, 2.0, 0.000001)
  }

  // test("backward 2") {

  // }

  // I don't think this can ever work.
  // Because if you do something like normalisation - e.g. row /  row.sum , the row.sum _is not an indepant constant_. Normalisation needs the jacobian gradient.
  // test("map rows") {
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

    val gradBack = graph.backward((tej = tej))

    // println(gradCalculated.printMat)

    for i <- 0 until mat.shape(0) do
      for j <- 0 until mat.shape(1) do
        assertEqualsDouble(
          gradBack.tej(i, j),
          matGrad(i, j),
          0.0000001
        )
      end for
    end for

  }

  test("arrange") {
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

    val graph = tej.arrange(indices)

    val gradBack = graph.backward((tej = tej))

    // println(gradCalculated.printMat)

    for i <- 0 until mat.shape(0) do
      for j <- 0 until mat.shape(1) do
        assertEqualsDouble(
          gradBack.tej(i, j),
          matGrad(i, j),
          0.0000001
        )
      end for
    end for
  }

  test("scalar plus") {
    import vecxt.BoundsCheck.DoBoundsCheck.yes
    given tejV: TejVGraph[Double] = TejVGraph[Double]()
    val a = TejV(Matrix.fromRows(Array(2.0, 4.0, 6.0)))
    val b = 3.0

    val res = (a + b.tej).sum
    // graphDebug(tejV.dag.toGraphviz)
    assertEquals(tejV.dag.getAllNodes.size, 4)
    assertEquals(tejV.dag.getAllEdges.size, 3)
    assertEqualsDouble(res.value.scalar, 21.0, 0.000001)

    val grad = res.backward((a = a))

    // assertEqualsDouble(out.a.scalar, 1.0, 0.000001)
    // assertEqualsDouble(out.b.scalar, 1.0, 0.000001)
  }

  test("unary log grad") {
    import vecxt.BoundsCheck.DoBoundsCheck.yes
    given tejV: TejVGraph[Double] = TejVGraph[Double]()
    val a = TejV(Matrix.fromRows(Array(2.0, 4.0, 6.0)))

    val res = (a + 1.tej).log.sum

    // assertEquals(tejV.dag.getAllNodes.size, 4)
    // assertEquals(tejV.dag.getAllEdges.size, )

    val grad = res.backward((a = a))

    assertEqualsDouble(grad.a.raw(0), 0.33333333, 0.000001)
    assertEqualsDouble(grad.a.raw(1), 0.2, 0.000001)
    assertEqualsDouble(grad.a.raw(2), 0.14285714, 0.000001)

    assertEqualsDouble(res.value.scalar, 4.653960, 0.000001)
  }

end DAGVSuite
