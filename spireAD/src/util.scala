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

  inline def jetArr(idxStart: Int = 0)(using jd: JetDim): NArray[Jet[Double]] =
    import spire.implicits.DoubleAlgebra
    import spire.implicits.ArrayNormedVectorSpace
    a.zipWithIndex.map((v, i) => Jet(v.toDouble) + Jet.h[Double](i + idxStart))
  end jetArr

  inline def jetArrNoGrad(using JetDim, ClassTag[T]): NArray[Jet[T]] =
    import spire.implicits.DoubleAlgebra
    import spire.implicits.ArrayNormedVectorSpace
    a.map(Jet(_))
  end jetArrNoGrad

  inline def tejArr(using
      inline jd: TejDim[Double],
      ct: ClassTag[Double],
      f: Field[Double]
  ): NArray[Tej[Double]] =
    a.map(d => Tej(d.toDouble))

end extension
