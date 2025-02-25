package io.github.quafadas.spireAD

import cats.kernel.laws.discipline.SemigroupTests
import org.scalacheck.Arbitrary
import cats.kernel.Eq
import org.scalacheck.Prop.forAll
import cats.kernel.laws.discipline.catsLawsIsEqToProp
import cats.data.NonEmptyList
import org.scalacheck.Gen

trait VectorisedMonoidTests[F[_], A] extends SemigroupTests[F[A]]:

  implicit def arbitraryNonEmptyList[A: Arbitrary](using arb: Arbitrary[A]): Arbitrary[NonEmptyList[A]] =
    Arbitrary(for
      a <- arb.arbitrary
      as <- Gen.listOf(arb.arbitrary)
    yield NonEmptyList(a, as))

  end arbitraryNonEmptyList

  def laws: VectorisedMonoidLaws[F, A]

  def monoid(implicit arbA: Arbitrary[F[A]], eqA: Eq[F[A]]): RuleSet =
    new DefaultRuleSet(
      "vectorised monoid",
      Some(semigroup),
      "left identity" -> forAll(laws.leftIdentity),
      "right identity" -> forAll(laws.rightIdentity),
      "combine all" -> forAll(laws.combineAll),
      "is id" -> forAll((a: F[A]) => laws.isId(a, eqA)),
      "repeat0" -> forAll(laws.repeat0)
    )
end VectorisedMonoidTests

object VectorisedMonoidTests:
  def apply[F[_], A](using vm: VectorisedMonoid[F, A]): VectorisedMonoidTests[F, A] =
    new VectorisedMonoidTests[F, A]:
      def laws: VectorisedMonoidLaws[F, A] = VectorisedMonoidLaws[F, A]

end VectorisedMonoidTests
