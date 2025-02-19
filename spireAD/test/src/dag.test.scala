package io.github.quafadas.spireAD

import munit.*

import spire.*
import spire.math.*
import spire.implicits.DoubleAlgebra
import spire.std.array.ArrayVectorSpace

import spire.algebra.Trig
import _root_.algebra.ring.Field

class DAGSuite extends FunSuite:

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
        |}"""

      // TODO renable
    // assertNoDiff(dag.toGraphviz.trim, expectedGraphviz.trim)
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

  def unaryTest(
      fct: Tej[Double] => Unit,
      fctJet: Jet[Double] => Jet[Double],
      opLabel: String
      // jetCheck: Jet[Double]
  )(using td: TejDim[Double], f: Field[Double], ct: ClassTag[Double]) =

    given jd: JetDim = td.jd
    val twoD = summon[Field[Double]].one * 2.0
    val two = Tej(twoD)
    val twoj = Jet(twoD) + Jet.h(0)
    // val one = Tej(summon[Field[Double]].one) // corrected back to one
    fct(two)
    assert(td.dag.toGraphviz.contains(opLabel))
    val sorted = td.dag.toposort.reverse
    assert(sorted.size == 2)

    sorted.head.grad = summon[Field[Double]].one

    for node <- sorted.reverse do td.dag.getNode(node.id).backward
    end for

    val forwardVersion = fctJet(twoj)
    assertEqualsDouble(
      sorted.last.grad,
      fctJet(twoj).infinitesimal(0),
      0.0000001
    )

    // topo.last
    // topo.last.backward()

    // assert(jetCheck.infinitesimal(0) ==)

  end unaryTest

  def binaryTest[Double: Trig: Field: ClassTag](
      fct: (Tej[Double], Tej[Double]) => Unit,
      opLabel: String
  )(using td: TejDim[Double]) =
    val one = Tej.one[Double]
    val zero = Tej.zero[Double]
    fct(one, zero)
    // println(td.dag.toGraphviz)
    assert(td.dag.toGraphviz.contains(opLabel))

    assertEquals(td.dag.toposort.size, 3)
  end binaryTest

  test("unary nodes : exp") {
    given td: TejDim[Double] = TejDim(1)
    given jd: JetDim = td.jd
    unaryTest(
      exp[Tej[Double]],
      exp[Jet[Double]],
      "Exp"
    )

    // assert(sorted.last == Jet())

  }

  test("unary nodes : sin") {
    given td: TejDim[Double] = TejDim(1)
    given jd: JetDim = td.jd
    unaryTest(sin[Tej[Double]], sin[Jet[Double]], "Sin")
  }

  test("unary nodes : log") {
    given td: TejDim[Double] = TejDim(1)
    given jd: JetDim = td.jd
    unaryTest(log[Tej[Double]], log[Jet[Double]], "Log")
  }

  test("unary nodes : cos") {
    given td: TejDim[Double] = TejDim(1)
    given jd: JetDim = td.jd
    unaryTest(cos[Tej[Double]], cos[Jet[Double]], "Cos")
  }

  test("binary nodes : +") {
    given td: TejDim[Double] = TejDim(1)
    binaryTest[Double]((x, y) => x + y, "+")
  }

  test("binary nodes : -") {
    given td: TejDim[Double] = TejDim(1)
    binaryTest[Double]((x, y) => x - y, "-")
  }

  test("binary nodes : *") {
    given td: TejDim[Double] = TejDim(1)
    binaryTest[Double]((x, y) => x * y, "Mul")
  }

  test("binary nodes : /") {
    given td: TejDim[Double] = TejDim(1)
    binaryTest[Double]((x, y) => x / y, "Div")
  }
end DAGSuite
