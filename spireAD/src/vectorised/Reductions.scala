package io.github.quafadas.spireAD.vectorised

import vecxt.all.*

object Reductions:

  given vta: Reductions[Array, Double] = new Reductions[Array, Double]:

    extension (a: Array[Double]) override def product: Double = vecxt.all.product(a)
    end extension

    extension (a: Array[Double]) inline def sum: Double = vecxt.all.sum(a)
    end extension

  given vtm: Reductions[Matrix, Double] = new Reductions[Matrix, Double]:

    extension (a: Matrix[Double]) override def product: Double = vecxt.all.product(a.raw)
    end extension

    extension (a: Matrix[Double]) inline def sum: Double = vecxt.all.sum(a.raw)

    end extension

end Reductions

trait Reductions[F[_], A]:
  // def e(x: A): A
  // def pi(x: A): A
  extension (a: F[A]) def sum: A
  end extension
  extension (a: F[A]) def product: A

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
end Reductions
