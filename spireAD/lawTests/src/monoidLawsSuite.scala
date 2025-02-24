package io.github.quafadas.spireAD

import spire.implicits.*
import munit.DisciplineSuite
import cats.kernel.laws.discipline.MonoidTests

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import cats.kernel.Eq
import cats.kernel.Monoid
import cats.instances.show
import cats.Show


// import org.scalacheck._
// import org.scalacheck.Gen._
// import org.scalacheck.Prop._

// val arrayGen: Int => Gen[Array[Int]] = (size: Int) => Gen.containerOfN[Array, Int](size, arbitrary[Int])

// val propSameSizeArrays = forAll(Gen.sized { size =>
//   for {
//     fixedSize <- Gen.const(size) // Capture the size once per test iteration
//     array1 <- arrayGen(fixedSize)
//     array2 <- arrayGen(fixedSize)
//   } yield (array1, array2)
// }) { case (arr1, arr2) =>
//   arr1.length == arr2.length // Ensure both arrays have the same size
// }

// propSameSizeArrays.check()



class WeirdMonoidSuite extends DisciplineSuite:

  given eq: Eq[Double] = new Eq:
    def eqv(x: Double, y: Double): Boolean =
      val tolerance = 0.000001 * math.max(math.abs(x), math.abs(y))
      math.abs(x - y) <= tolerance
    end eqv

  given eqArr(using eqvD: Eq[Double]): Eq[Array[Double]] = new Eq:
    def eqv(x: Array[Double], y: Array[Double]): Boolean =
      x.zip(y).forall((a, b) => eqvD.eqv(a, b))    
    end eqv

  given eqVec(using eqvD: Eq[Double]): Eq[Vector[Double]] = new Eq:
    def eqv(x: Vector[Double], y: Vector[Double]): Boolean =
      x.zip(y).forall((a, b) => eqvD.eqv(a, b))
    end eqv

  // checkAll("Double", MonoidTests[Array[Double]].monoid)  

  given show : Show[Array[Double]] = new Show[Array[Double]] {
    def show(a: Array[Double]): String = a.mkString("[" , ", ", ")")
  }

  given a2:  Arbitrary[Array[Double]] = Arbitrary(
    Gen.sized { sized =>      
      for {
        size <- Gen.const(sized + 1) // Choose a size for the array
        array <- Gen.containerOfN[Array, Double](size, Gen.choose(-1000.0, 1000.0))
      } yield array
    }
  )

  given aV:  Arbitrary[Vector[Double]] = Arbitrary(
    Gen.sized { sized =>      
      for {
        size <- Gen.const(sized + 1) // Choose a size for the array
        array <- Gen.containerOfN[Vector, Double](size, Gen.choose(-1000.0, 1000.0))
      } yield array
    }
  )


  import VectorisedMonoid.additiveVectorMonoid
  checkAll("Addidditve Vector Monoid", VectorisedMonoidTests[Vector, Double].monoid)

end WeirdMonoidSuite
