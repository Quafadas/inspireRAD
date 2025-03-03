package io.github.quafadas.spireAD

import munit.*

import spire.*
import spire.math.*
import spire.implicits.DoubleAlgebra
import spire.std.array.ArrayVectorSpace

import _root_.algebra.ring.Field

class DAGVSuite extends FunSuite:

  test("Add and retrieve nodes") {
    val dag = new DAGV[Double, VNode[?, Double]]()
    val tejV = TejV[Double, VNode[?, Double]]()
    tejV.assertEquals(
      dag.getAllNodes.asInstanceOf[Set[DebugNode[Double]]],
      Set(DebugNode("A"), DebugNode("B"))
    )
  }

end DAGVSuite
