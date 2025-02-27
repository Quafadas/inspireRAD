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

class MultiplicateMonoidSuite extends DisciplineSuite:

  import VectorisedMultiplicativeMonoids.multiplicativeArrayMonoid
  checkAll("Addidditve Double Array Monoid", VectorisedMonoidTests[Array, Double].monoid)
end MultiplicateMonoidSuite
