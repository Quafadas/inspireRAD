import munit._
import spire._
import spire.math._
import spire.implicits._
import _root_.algebra.ring.Field
import spire.algebra.Trig
import spire.syntax.rig
import _root_.algebra.ring.Rig
import vecxt.all.`*`

// import Tej.*
// import scala.math.Fractional.Implicits.infixFractionalOps
// import scala.math.Integral.Implicits.infixIntegralOps
// import scala.math.Numeric.Implicits.infixNumericOps

import vecxt.BoundsCheck.DoBoundsCheck.yes
import spire.algebra.VectorSpace
import spire.algebra.NRoot
import cats.kernel.Eq

class TejSuite extends FunSuite {

  def assertEqualsTejToJet[T: Eq](t: Tej[T], j: Jet[T]) =
    assertEquals(t.j.real, j.real)
    for (i, ji) <- t.j.infinitesimal.zip(j.infinitesimal) do assertEquals(i, ji)

  test("jet addition") {
    given jd: JetDim = JetDim(2)
    val x = Jet(1.0) + Jet.h[Double](0)
    val y = Jet(1.0) + Jet.h[Double](1)
    val result = x + y
    assertEquals(result, Jet(2.0, Array(1.0, 1.0)))

  }

  test("jet multiplication") {
    given jd: JetDim = JetDim(2)
    val x = Jet(1.0) + Jet.h[Double](0)
    val y = Jet(1.0) + Jet.h[Double](1)
    val result = x * y
    assertEquals(result, Jet(1.0, Array(1.0, 1.0)))
  }

  test("jet equality") {
    given jd: JetDim = JetDim(2)
    val x = Jet(1.0)
    val y = Jet(1.0)
    assertEquals(x, y)
  }

  /** This is essentially the "forward" mode of automatic differentiation.
    */

  test("jet function composition") {
    given jd: JetDim = JetDim(2)
    val x = Jet(1.0) + Jet.h[Double](0)
    val y = Jet(1.0) + Jet.h[Double](1)
    val result = x * y

    def messy[T: Field: Trig](x: T, y: T): T = x * x + exp(y)

    val messyResult = messy(x, y)

    assertEquals(messyResult, Jet(1.0 + exp(1), Array(2, exp(1))))
  }

  test("Tej addition") {
    given jd: TejDim[Double] = TejDim(2)
    val x: Tej[Double] = 1.0 + Tej.h[Double](0)
    val y: Tej[Double] = 1.0 + Tej.h[Double](1)
    // println(jd.dag.toGraphviz)
    val result = x + y
    assertEquals(result, Tej(2.0, Array(1.0, 1.0)))
  }

  test("Tej multiplication") {
    given jd: TejDim[Double] = TejDim(2)
    val x: Tej[Double] = 1.0 + Tej.h[Double](0)
    val y: Tej[Double] = 1.0 + Tej.h[Double](1)
    val result = x * y
    assertEquals(result, Tej(1.0, Array(1.0, 1.0)))
  }

  test("Tej equality") {
    given jd: TejDim[Double] = TejDim(2)
    val x = Tej(1.0)
    val y = Tej(1.0)
    assertEquals(x, y)
  }

  test("zero Tej") {
    given jd: TejDim[Double] = TejDim(2)
    val zero = Tej.zero[Double]
    assertEquals(zero, Tej(0.0, Array(0.0, 0.0)))
  }

  test("sin") {
    given td: TejDim[Double] = TejDim(1)
    given jd: JetDim = td.jd
    val xT = Tej(1.0)
    val xJ = Jet(1.0)
    def sinT[T: Trig](t: T) = sin(t)
    assertEqualsTejToJet(sinT(xT), sinT(xJ))
  }

  test("log") {
    given td: TejDim[Double] = TejDim(1)
    given jd: JetDim = td.jd
    val xT = Tej(1.0)
    val xJ = Jet(1.0)
    def logT[T: Trig](t: T) = log(t)
    assertEqualsTejToJet(logT(xT), logT(xJ))
  }

  // test("Tej constructor doesn't compile through Jet") {
  //   given td: TejDim[Double] = TejDim(1)
  //   given jd: JetDim = td.jd
  //   val x = Tej(Jet(1.0))
  // }

  test("construct digraph") {
    given td: TejDim[Double] = TejDim(2)

    val t1 = Tej(1.0) + Tej.h[Double](0)
    val t2 = Tej(2.0) + Tej.h[Double](1)

    def sq[T: Field: Trig](x: T): T = x * x

    def ttim[T: Field: Trig](x: T, y: T): T = x * y

    val ttimImpl = ttim(t1, t2)

    // println(ttimImpl)
    // println(td.dag.toGraphviz)

  }

}
