package io.github.quafadas.spireAD

import spire.math.JetDim
import spire.math.Jet
import scala.reflect.ClassTag
import scala.math.Numeric.Implicits.infixNumericOps
import algebra.ring.Field

extension [T: Numeric: Field](a: Array[T])
  def jetArr(using jd: JetDim): Array[Jet[Double]] =
    import spire.implicits.DoubleAlgebra
    import spire.implicits.ArrayNormedVectorSpace
    a.zipWithIndex.map((v, i) => Jet(v.toDouble) + Jet.h[Double](i))
  end jetArr

  def tejArr(using
      jd: TejDim[Double],
      ct: ClassTag[Double],
      f: Field[Double]
  ): Array[Tej[Double]] =
    a.zipWithIndex.map(d => Tej(d._1.toDouble))
end extension
