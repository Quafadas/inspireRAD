import munit._

import spire._
import spire.math._
import spire.implicits.DoubleAlgebra
import spire.std.array.ArrayVectorSpace

import spire.algebra.Trig
import _root_.algebra.ring.Field

class DAGSuite extends FunSuite {

  test("Add and retrieve nodes") {
    val dag = new DAG[Double]()
    dag.addStringNode("A")
    dag.addStringNode("B")

    assertEquals(
      dag.getAllNodes.asInstanceOf[Set[DebugNode[Double]]],
      Set(DebugNode("A"), DebugNode("B"))
    )
  }

  test("Add edges and check existence") {
    val dag = new DAG[Double]()
    val dna = DebugNode("A")
    val dnb = DebugNode("B")
    dag.addNode(dna)
    dag.addNode(dnb)
    dag.addEdge(dna, dnb)

    assert(dag.hasEdge(dna, dnb))
    assert(!dag.hasEdge(dnb, dna))
  }

  test("Remove nodes and check graph persistence") {
    val dag = new DAG[Double]()
    val dna = DebugNode("A")
    val dnb = DebugNode("B")
    dag.addNode(dna)
    dag.addNode(dnb)
    dag.addEdge(dna, dnb)
    dag.removeNode(dna)

    assertEquals(dag.getAllNodes, Set[AdNode[Double]](dnb))
    assert(!dag.hasEdge(dna, dnb))
  }

  test("Remove edges and confirm") {
    val dag = new DAG[Double]()
    val dna = DebugNode("A")
    val dnb = DebugNode("B")
    dag.addNode(dna)
    dag.addNode(dnb)
    dag.addEdge(dna, dnb)
    dag.removeEdge(dna, dnb)

    assert(!dag.hasEdge(dna, dnb))
  }

  test("Topological sort with acyclic graph") {
    val dag = new DAG[Double]()
    val dna = DebugNode("A")
    val dnb = DebugNode("B")
    val dnc = DebugNode("C")
    dag.addNode(dnc)
    dag.addNode(dnb)
    dag.addNode(dna)
    dag.addEdge(dna, dnb)
    dag.addEdge(dnb, dnc)

    val sorted = dag.toposort
    assertEquals(sorted, List(dna, dnb, dnc))
  }

  test("Topological sort with cyclic graph throws exception") {
    val dag = new DAG[Double]()
    val dna = DebugNode("A")
    val dnb = DebugNode("B")
    dag.addNode(dna)
    dag.addNode(dnb)
    dag.addEdge(dna, dnb)
    dag.addEdge(dnb, dna)

    intercept[IllegalArgumentException] {
      dag.toposort
    }
  }

  test("Graphviz representation correctness") {
    val dag = new DAG[Double]()
    val dna = DebugNode("A")
    val dnb = DebugNode("B")
    dag.addNode(dna)
    dag.addNode(dnb)
    dag.addEdge(dna, dnb)

    // println(dag.toGraphviz.trim)

    val expectedGraphviz =
      """digraph {
        |  "A" -> "B";
        |  "B";
        |}""".stripMargin

    assertNoDiff(dag.toGraphviz.trim, expectedGraphviz.trim)
  }

  test("Graph is empty") {
    val dag = new DAG[Double]()
    val dna = DebugNode("A")
    assert(dag.isEmpty)

    dag.addNode(dna)
    assert(!dag.isEmpty)

    dag.removeNode(dna)
    assert(dag.isEmpty)
  }

  def unaryTest[Double: Field: Trig: ClassTag](
      fct: (Tej[Double]) => Unit
  )(using td: TejDim[Double]) =
    val one = Tej.one[Double]
    fct(one)
    assertEquals(td.dag.toposort.size, 2)

  def binaryTest[Double: Trig: Field: ClassTag](
      fct: (Tej[Double], Tej[Double]) => Unit
  )(using td: TejDim[Double]) =
    val one = Tej.one[Double]
    val zero = Tej.zero[Double]
    fct(one, zero)
    // println(td.dag.toGraphviz)

    assertEquals(td.dag.toposort.size, 3)

  test("unary nodes : exp") {
    given td: TejDim[Double] = TejDim(1)
    unaryTest(exp[Tej[Double]])

  }

  test("unary nodes : sin") {
    given td: TejDim[Double] = TejDim(1)
    unaryTest(sin[Tej[Double]])
  }

  test("unary nodes : log") {
    given td: TejDim[Double] = TejDim(1)
    unaryTest(log[Tej[Double]])
  }

  test("unary nodes : cos") {
    given td: TejDim[Double] = TejDim(1)
    unaryTest(cos[Tej[Double]])
  }

  test("binary nodes : +") {
    given td: TejDim[Double] = TejDim(1)
    binaryTest[Double]((x, y) => x + y)
  }

  test("binary nodes : -") {
    given td: TejDim[Double] = TejDim(1)
    binaryTest[Double]((x, y) => x - y)
  }

  test("binary nodes : *") {
    given td: TejDim[Double] = TejDim(1)
    binaryTest[Double]((x, y) => x * y)
  }

  test("binary nodes : /") {
    given td: TejDim[Double] = TejDim(1)
    binaryTest[Double]((x, y) => x / y)
  }

}
