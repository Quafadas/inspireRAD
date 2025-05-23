package io.github.quafadas.spireAD

import vecxt.all.*
import narr.*

object VectorisedTrig:

  given vts: VectorisedTrig[Scalar, Double] = new VectorisedTrig[Scalar, Double]:

    extension (a: Scalar[Double]) override def exp: Scalar[Double] = Scalar(Math.exp(a.scalar))
    end extension

    extension (a: Scalar[Double]) override def log: Scalar[Double] = Scalar(Math.log(a.scalar))
    end extension

    extension (a: Scalar[Double]) override def sin: Scalar[Double] = Scalar(Math.sin(a.scalar))
    end extension

    extension (a: Scalar[Double]) override def cos: Scalar[Double] = Scalar(Math.cos(a.scalar))
    end extension

  given vta: VectorisedTrig[NArray, Double] = new VectorisedTrig[NArray, Double]:
    extension (a: NArray[Double])
      inline def exp: NArray[Double] = vecxt.all.exp(a)
      inline def log: NArray[Double] = vecxt.all.log(a)
      inline def sin: NArray[Double] = vecxt.all.sin(a)
      inline def cos: NArray[Double] = vecxt.all.cos(a)
    end extension

  given vtm: VectorisedTrig[Matrix, Double] = new VectorisedTrig[Matrix, Double]:
    extension (a: Matrix[Double])
      inline def exp: Matrix[Double] = vecxt.all.exp(a)
      inline def log: Matrix[Double] = vecxt.all.log(a)
      inline def sin: Matrix[Double] = vecxt.all.sin(a)
      inline def cos: Matrix[Double] = vecxt.all.cos(a)
    end extension

end VectorisedTrig

trait VectorisedTrig[F[_], A]:
  // def e(x: A): A
  // def pi(x: A): A
  extension (a: F[A])
    def exp: F[A]
    def log: F[A]
    def sin: F[A]
    def cos: F[A]
  end extension

  // def exp(a: F[A]): F[A]
  // // def expm1(a: A): A
  // def log(a: F[A]): F[A]
  // // def log1p(a: A): A

  // def sin(a: F[A]): F[A]
  // def cos(a: A): A
  // def tan(a: A): A

  // def asin(a: A): A
  // def acos(a: A): A
  // def atan(a: A): A
  // def atan2(y: A, x: A): A

  // def sinh(x: A): A
  // def cosh(x: A): A
  // def tanh(x: A): A

  // def toRadians(a: A): A
  // def toDegrees(a: A): A
end VectorisedTrig
