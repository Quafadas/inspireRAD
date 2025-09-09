package io.github.quafadas.inspireRAD

import scala.reflect.ClassTag

trait LossContext[Matrix[_], Scalar[_], Array[_], T](
  using
    val mOps: Matrixy[Matrix, T],
    val fm: VectorisedField[Matrix, T],
    val fa: VectorisedTrig[Array, T],
    val fas: VectorisedField[Scalar, T],
    val faa: VectorisedField[Array, T],
    val redArr: Reductions[Array, T, 1],
    val redMat: Reductions[Matrix, T, 2],
    val t: VectorisedTrig[Matrix, T],
    val nt: Numeric[T],
    val ct: ClassTag[T]
)

object LossContext:
  inline given apply[Matrix[_], Scalar[_], Array[_], T](
    using
      Matrixy[Matrix, T],
      VectorisedField[Matrix, T],
      VectorisedTrig[Array, T],
      VectorisedField[Scalar, T],
      VectorisedField[Array, T],
      Reductions[Array, T, 1],
      Reductions[Matrix, T, 2],
      VectorisedTrig[Matrix, T],
      Numeric[T],
      ClassTag[T]
  ): LossContext[Matrix, Scalar, Array, T] =
    new LossContext[Matrix, Scalar, Array, T] {}
