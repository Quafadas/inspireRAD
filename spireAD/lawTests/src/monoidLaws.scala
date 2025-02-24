package io.github.quafadas.spireAD

import spire.implicits.*
import munit.DisciplineSuite
import cats.kernel.laws.discipline.MonoidTests

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import cats.kernel.Eq

class WeirdMonoidSuite extends DisciplineSuite:

  given eq: Eq[Double] = new Eq:
    def eqv(x: Double, y: Double): Boolean =
      val tolerance = 0.000001 * math.max(math.abs(x), math.abs(y))
      math.abs(x - y) <= tolerance
    end eqv

  checkAll("Double", MonoidTests[Array[Double]].monoid)

  checkAll("Additive semigroup", AdditiveSemigroupTests[Array[Double]].additiveSemigroup)

end WeirdMonoidSuite
