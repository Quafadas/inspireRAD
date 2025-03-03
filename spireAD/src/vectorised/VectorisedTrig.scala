package io.github.quafadas.spireAD

import vecxt.all.*

object VectorisedTrig:

  given vta: VectorisedTrig[Array, Double] = new VectorisedTrig[Array, Double]:
    extension (a: Array[Double])
      inline def exp: Array[Double] = vecxt.all.exp(a)
      inline def log: Array[Double] = vecxt.all.log(a)
      inline def sin: Array[Double] = vecxt.all.sin(a)
    end extension

  given vtm: VectorisedTrig[Matrix, Double] = new VectorisedTrig[Matrix, Double]:
    extension (a: Matrix[Double])
      inline def exp: Matrix[Double] = vecxt.all.exp(a)
      inline def log: Matrix[Double] = vecxt.all.log(a)
      inline def sin: Matrix[Double] = vecxt.all.sin(a)
    end extension

end VectorisedTrig

trait VectorisedTrig[F[_], A]:
  // def e(x: A): A
  // def pi(x: A): A
  extension (a: F[A])
    def exp: F[A]
    def log: F[A]
    def sin: F[A]
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
