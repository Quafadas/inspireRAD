package io.github.quafadas.inspireRAD

import spire.implicits.*
import munit.DisciplineSuite
import cats.kernel.laws.discipline.MonoidTests

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import cats.kernel.Eq
import cats.kernel.Monoid
import cats.instances.show
import cats.Show

given eq: Eq[Double] = new Eq:
  def eqv(x: Double, y: Double): Boolean =
    val tolerance = 0.00000001 * math.max(math.abs(x), math.abs(y))
    math.abs(x - y) <= tolerance
  end eqv

def fEq[A, F <: Iterable](using eqvD: Eq[A]) = new Eq[F[A]]:
  def eqv(x: F[A], y: F[A]): Boolean =
    x.zip(y).forall((a, b) => eqvD.eqv(a, b))
  end eqv

def arrDEqv[A](using eqvD: Eq[A]): Eq[Array[A]] = new Eq[Array[A]]:
  def eqv(x: Array[A], y: Array[A]): Boolean =
    x.zip(y).forall((a, b) => eqvD.eqv(a, b))

given eqArr: Eq[Array[Double]] = arrDEqv[Double]
given eqV: Eq[Vector[Double]] = fEq[Double, Vector](using eq)
given eqVI: Eq[Vector[Int]] = fEq[Int, Vector]
given eqL: Eq[List[Double]] = fEq[Double, List](using eq)

given a2: Arbitrary[Array[Double]] = Arbitrary(
  Gen.sized { sized =>
    for
      size <- Gen.const(sized + 1) // Choose a size for the array
      array <- Gen.containerOfN[Array, Double](size, Gen.double)
    yield array
  }
)

given aV: Arbitrary[Vector[Double]] = Arbitrary(
  Gen.sized { sized =>
    for
      size <- Gen.const(sized + 1) // Choose a size for the array
      array <- Gen.containerOfN[Vector, Double](size, Gen.double)
    yield array
    end for
  }
)

given aVI: Arbitrary[Vector[Int]] = Arbitrary(
  Gen.sized { sized =>
    for
      size <- Gen.const(sized + 1) // Choose a size for the array
      array <- Gen.containerOfN[Vector, Int](size, Gen.chooseNum(Int.MinValue, Int.MaxValue))
    yield array
    end for
  }
)
