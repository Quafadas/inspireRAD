package io.github.quafadas.spireAD

/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

import cats.kernel.Eq
import cats.kernel.laws.SemigroupLaws
import cats.kernel.laws.IsEq
import cats.kernel.laws.IsEqArrow
import io.github.quafadas.spireAD.VectorisedMonoid

import cats.data.NonEmptyList

trait VectorisedMonoidLaws[F[_], A] extends SemigroupLaws[F[A]]:
  implicit override def S: VectorisedMonoid[F, A]

  def leftIdentity(x: F[A]): IsEq[F[A]] =
    S.combine(S.empty(x), x) <-> x

  def rightIdentity(x: F[A]): IsEq[F[A]] =
    S.combine(x, S.empty(x)) <-> x

  def repeat0(x: F[A]): IsEq[F[A]] =
    S.combineN(x, 0) <-> S.empty(x)

    /** This law would not be valid for a "vectorised monoid", which must an instance to combine. This will throw an
      * exception in the wild - and that is expected behaviour.
      */

  // def collect0: IsEq[F[A]] =
  //   S.combineAll(Nil) <-> S.empty

  def combineAll(xs: NonEmptyList[F[A]]): IsEq[F[A]] =
    val head = xs.head
    S.combineAll(xs.iterator) <-> (S.empty(head) +: head +: xs.tail).reduce(S.combine)
  end combineAll

  def isId(x: F[A], eqv: Eq[F[A]]): IsEq[Boolean] =
    eqv.eqv(x, S.empty(x)) <-> S.isEmpty(x)(eqv)

end VectorisedMonoidLaws

object VectorisedMonoidLaws:
  def apply[F[_], A](implicit ev: VectorisedMonoid[F, A]): VectorisedMonoidLaws[F, A] =
    new VectorisedMonoidLaws[F, A]:
      def S: VectorisedMonoid[F, A] = ev
end VectorisedMonoidLaws
