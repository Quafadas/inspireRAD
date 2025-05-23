package io.github.quafadas.spireAD

import spire.math.JetDim
import spire.math.Jet
import scala.reflect.ClassTag
import scala.math.Numeric.Implicits.infixNumericOps
import algebra.ring.Field
import narr.*

extension [T: Numeric: Field](a: NArray[T])
  inline def jetArr(using jd: JetDim): NArray[Jet[Double]] =
    import spire.implicits.DoubleAlgebra
    import spire.implicits.ArrayNormedVectorSpace
    a.zipWithIndex.map((v, i) => Jet(v.toDouble) + Jet.h[Double](i))
  end jetArr

  inline def tejArr(using
      inline jd: TejDim[Double],
      ct: ClassTag[Double],
      f: Field[Double]
  ): NArray[Tej[Double]] =
    a.zipWithIndex.map(d => Tej(d._1.toDouble))
end extension
