package io.github.quafadas.inspireRAD

import scala.reflect.ClassTag
import narr.NArray
import vecxt.matrix.Matrix

trait LossContext[Matrix[_], Array[_], Scalar[_],  T]:
    given mOps: Matrixy[Matrix, T]
    given fm: VectorisedField[Matrix, T]
    given fa: VectorisedTrig[Array, T]
    given fas: VectorisedField[Scalar, T]
    given faa: VectorisedField[Array, T]
    given redArr: Reductions[Array, T, 1]
    given redMat: Reductions[Matrix, T, 2]
    given t: VectorisedTrig[Matrix, T]
    given nt: Numeric[T]
    given ct: ClassTag[T]

object LossContext:
    // Convenience given for Double types  
    given forDouble: LossContext[Matrix, NArray, Scalar, Double] =
        new LossContext[Matrix, NArray, Scalar, Double]:
            given mOps: Matrixy[Matrix, Double] = Matrixy.matOps
            given fm: VectorisedField[Matrix, Double] = VectorisedField.elementwiseMatrixDoubleField
            given fa: VectorisedTrig[NArray, Double] = VectorisedTrig.vta
            given fas: VectorisedField[Scalar, Double] = VectorisedField.scalarField
            given faa: VectorisedField[NArray, Double] = VectorisedField.elementwiseArrayDoubleField
            given redArr: Reductions[NArray, Double, 1] = Reductions.redArray
            given redMat: Reductions[Matrix, Double, 2] = Reductions.redMat
            given t: VectorisedTrig[Matrix, Double] = VectorisedTrig.vtm
            given nt: Numeric[Double] = summon[Numeric[Double]]
            given ct: ClassTag[Double] = summon[ClassTag[Double]] 
