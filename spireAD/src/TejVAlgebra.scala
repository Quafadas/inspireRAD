package io.github.quafadas.spireAD

import scala.math.*
import scala.reflect.*
import scala.NamedTuple.*
import spire.algebra.*
import scala.util.chaining.*
import scala.specialized as sp
import java.util.UUID
import cats.Show
import vecxt.matrix.Matrix
import narr.*
import vecxt.BoundsCheck


object TejVDoubleAlgebra:

  import Matrixy.matOps

  given tejDoubleScalar: TejVAlgebra[Scalar, Double] = TejVAlgebra[Scalar, Double]()
  given tejDoubleArray: TejVAlgebra[NArray, Double] = TejVAlgebra[NArray, Double]()
  given tejDoubleMatrix: MatrixAlg[Matrix, Double] = MatrixAlg[Matrix, Double]()

class MatrixAlg[F[_], T](using
    m: Matrixy[F, T],
    c: ClassTag[T],
    t: VectorisedTrig[F, T],
    f: VectorisedField[F, T]
  ) extends TejVAlgebra[F, T], Matrixy[F, T] :
  export m.*

end MatrixAlg

class TejVAlgebra[F[_], T](using
    c: ClassTag[T],
    t: VectorisedTrig[F, T],
    f: VectorisedField[F, T]
) extends VectorisedTrig[F, T],
    VectorisedField[F, T]:
  export t.*

  // export f.`*`

  export f.`*`
  export f.`+`
  export f.`-`
  export f.`/`
  export f.*:*
  export f.one
  export f.abs
  export f.numel
  export f.zero
  export f.>
  export f.allOnes
  export f.fromDouble
  export f.clampMin
  export f.const
  export f.productExceptSelf
  export f.sum
  export f.sign
  export f.unary_-

  override val numDimensions: Int = f.numDimensions



end TejVAlgebra