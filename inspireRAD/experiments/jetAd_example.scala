// import spire._
// import spire.math._
// import spire.implicits.*
// import spire.algebra.Trig
// import _root_.algebra.ring.Field

// import vecxt.arrays.*
// import scala.Array.ArrayFactory
// import vecxt.BoundsCheck.*
// import vecxt.BoundsCheck.DoBoundsCheck.yes
// import vecxt.dimMatCheck
// import spire.algebra.NRoot
// import vecxt.matrix.Matrix
// import vecxt.all.eye
// import vecxt.all.ones
// import JetMatrixTypeClasses.JetMatDim

// @main def jetAd_example =

//   // given nroot: NRoot[Array[Double]] = JetAd_TC.nrootAD

//   // given jetARing: JetADIsRing = new JetADIsRing {

//   //   override implicit def fd: Field[Double] = ???

//   //   override implicit def fad: Field[Array[Double]] = JetAd_TC.fadAD

//   //   override implicit def d: JetADDim = jd2

//   // }
//   // given jetField: JetADIsField = new JetADIsField {

//   //   override implicit def fd: Field[Double] = ???

//   //   override implicit def fad: Field[Array[Double]] = JetAd_TC.fadAD

//   //   override implicit def d: JetADDim = jd2

//   // }

//   import Reducible.rd
//   import Reducible.rjd
//   import Reducible.rarrj
//   import Reducible.ramd
//   import JetArrayTypeClasses.arrjAD
//   import JetArrayTypeClasses.fadAD
//   import JetArrayTypeClasses.trigAD
//   import JetArrayTypeClasses.nrootAD
//   import JetArrayTypeClasses.signed

//   import JetMatrixTypeClasses.arrjAD
//   import JetMatrixTypeClasses.fadAD
//   import JetMatrixTypeClasses.trigAD
//   import JetMatrixTypeClasses.nrootAD
//   import JetMatrixTypeClasses.signed
//   import vecxt.matrixUtil.printMat

//   given jd: JetDim = JetDim(1)

//   val x: Jet[Double] = 1.0 + Jet.h[Double](0)
//   val y: Jet[Double] = 2.0 + Jet.h[Double](1)
//   val z: Jet[Double] = 3.0 + Jet.h[Double](2)

//   given jad: JetADDim = JetADDim((3, 3))
//   given jmat: JetMatDim = JetMatDim((2, 2))

//   def f[@specialized(Double) T: Field: Trig: NRoot](x: T, y: T): T = exp(
//     -Field[T].pow((sin(x) - cos(y)), 2)
//   )

//   def g[F[_], @specialized(Double) T: Field: Trig: NRoot](x: T, y: T): T =
//     x + y + exp(x * y)

//   def g3[F[_], @specialized(Double) T: Field: Trig: NRoot](
//       x: T,
//       y: T,
//       z: T
//   ): T =
//     x + y + z + log(x * y * z)

//   def gVec[F[_], @specialized(Double) T: Field: Trig: NRoot](x: F[T])(using
//       r: Reducible[F, T]
//   ): T =
//     x.sum + x.product.log

//   def gVec2(
//       x: Jet[Matrix[Double]],
//       y: Jet[Matrix[Double]]
//   ): Jet[Matrix[Double]] =
//     x * y

//   println("Pure: x = 1.0, y = 2.0")
//   println(g[cats.Id, Double](1.0, 2.0))
//   println

//   println("Jet: x = 1.0, y = 2.0")
//   println(g3[cats.Id, Jet[Double]](x, y, z))
//   println

//   println("Vectorised form")
//   println("TARGET---- ")
//   val arr = Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
//   val jetArr2 = arr.zipWithIndex.map((v, i) => Jet(v) + Jet.h[Double](i))
//   println(gVec[Array, Double](arr))

//   println(gVec(jetArr2))
//   println

//   println("MATRIX -------")
//   println(
//     gVec[Matrix, Double](Matrix(Array(1.0, 2.0, 3.0, 4.0), (2, 2)))
//   )
//   println

//   println("MATRIX2 -------")
//   val out = gVec2(
//     Jet(Matrix(Array(1.0, 2.0, 3.0, 4.0), (2, 2))),
//     Jet(Matrix(Array(1.0, 2.0, 3.0, 4.0), (2, 2)))
//   )

//   println(out.real.printMat)
//   println(out.infinitesimal(1).printMat)
//   println(out.infinitesimal(2).printMat)
//   println

//   // println(
//   //   gVec[Matrix, Jet[Double]](
//   //     Matrix(jetArr2, (3, 2))
//   //   )
//   // )

//   println

//   println("Pure Jets -------")
//   println(gVec(Array(x, y, z)))
//   println

//   println("Jet AD -------")
//   val jetArr = JetAD(Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), Matrix.eye[Double](6))
//   println(gVec[Id, JetAD](jetArr))

//   // given td: TejDim = TejDim(2)

//   // val xT = Tej(1.0) + Tej.h[Double](0)
//   // val yT = Tej(2.0) + Tej.h[Double](1)

//   // println(g(xT, yT))
//   // println(td.dag.toGraphviz)

//   // def soA(
//   //     x: Array[Double],
//   //     y: Array[Double]
//   // )(using fad: Field[Array[Double]], tad: Trig[Array[Double]]): Array[Double] =
//   //   val i = x * y

//   //   val j = sin(x)
//   //   i + j

//   // def g[@specialized(Double) T: Field: Trig](x: T, y: T): T =
//   //   sin(log(exp(x) + exp(-y)) - x / cos(y))

//   // // https://stats.stackexchange.com/questions/224140/step-by-step-example-of-reverse-mode-automatic-differentiation
//   // def so[@specialized(Double) T: Field: Trig](x: T, y: T): T =
//   //   x * y + sin(x)

//   // def softmax[@specialized(Double) T: Field: Trig](x: T, y: T): T =
//   //   x * y /

//   // for i <- 0 to 3 do
//   //   val x = i + Jet.h[Double](0)
//   //   val y = i + Jet.h[Double](1)

//   //   val z = so(x, y)
//   //   println(z)

//   // val result = so(xa, ya)
//   // println(result.mkString(", "))

//   // val z = so(x, y)

//   // def sq[T: Field](x: T): T = x * x
//   // def sqExp[T: Field: Trig](x: T): T =
//   //   (x * x + exp(x) - log(x) + exp(sin(x))) / x

//   // given jd: JetDim = JetDim(1)
//   // val simpleJet1 = Jet(1.0) + Jet.h[Double](0)
//   // val simpleJet2 = Jet(2.0) + Jet.h[Double](0)
//   // val xa = Array[Double](1, 2, 3)
//   // val ya = Array[Double](1, 2, 3)

//   // println("Seuare only")
//   // println(sq(simpleJet1))
//   // println(sq(simpleJet2))

//   // import JetAd_TC.signed
//   // given jd2: JetADDim = JetADDim((2, 1))

//   // val xJD: JetAD = JetAD(Array[Double](1.0, 2.0), Matrix.ones[Double]((1, 2)))
//   // val zAD = sq(xJD)

//   // println(zAD)
//   // println("With Exp")

//   // println(sqExp(simpleJet1))
//   // println(sqExp(simpleJet2))

//   // println(sqExp(xJD))
