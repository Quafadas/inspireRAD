package io.github.quafadas.spireAD

import vecxt.all.*

object Reductions:

  given vta: Reductions[Array, Double] = new Reductions[Array, Double]:

    extension (a: Array[Double])
      override def product: Double = vecxt.all.product(a)
      inline def mean: Double = vecxt.all.mean(a)
      inline def sum: Double = vecxt.all.sum(a)
    end extension

  given vtm: Reductions[Matrix, Double] = new Reductions[Matrix, Double]:

    extension (a: Matrix[Double])
      def mean: Double = vecxt.all.sum(a.raw) / a.raw.length
      def product: Double = vecxt.all.product(a.raw)
      inline def sum: Double = vecxt.all.sum(a.raw)
    end extension

end Reductions

trait Reductions[F[_], A]:
  extension (a: F[A])
    def sum: A
    def product: A
    def mean: A

  end extension

end Reductions
