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
// import vecxt.matrixUtil.printMat
// import vecxt.all.fromRows
// import cats.kernel.Order
// import _root_.algebra.ring.Signed
// import spire.algebra.VectorSpace
// import spire.std.ArrayVectorSpace

// @main def matJetExample =

//   // import Reducible.radjMat
//   import JetMatrixTypeClasses.arrjAD
//   import JetMatrixTypeClasses.fadAD
//   import JetMatrixTypeClasses.trigAD
//   import JetMatrixTypeClasses.nrootAD
//   import JetMatrixTypeClasses.signed
//   import JetMatrixTypeClasses.orderAD

//   import spire.math.Jet.*

//   import vecxt.matrixUtil.printMat

//   given jd: JetDim = JetDim(2)

//   given jmat: JetMatDim = JetMatDim((2, 2))

//   val ji: JetInstances = new JetInstances {}
//   given nrootJetT[T: NRoot: ClassTag: Field: Order: Signed: Trig]
//       : NRoot[Jet[T]] =
//     ji.JetAlgebra[T]

//   given vs[T: ClassTag: Field: NRoot: Order: Signed: Trig]
//       : VectorSpace[Array[T], T] =
//     import spire.std.array.ArrayVectorSpace
//     implicitly[VectorSpace[Array[T], T]]

//   def gVec2[T: Field: ClassTag: Trig](
//       x: Jet[T],
//       y: Jet[T]
//   )(using vs: VectorSpace[Jet[T], T]): Jet[T] =
//     x.exp + y.exp

//   println("scalar")
//   println(
//     gVec2(
//       Jet(1.0, 0),
//       Jet(2.0, 0)
//     )
//   )

//   def gVecMat[T: Field: ClassTag: Trig](
//       x: Jet[Matrix[Double]],
//       y: Jet[Matrix[Double]]
//   )(using
//       vs: VectorSpace[Jet[T], T],
//       f: Field[Jet[Matrix[Double]]],
//       t: Trig[Jet[Matrix[Double]]],
//       r: Reducible[Id, Jet[Matrix[Double]]]
//   ): Jet[Matrix[Double]] =
//     (sin(x) + y.pow(2)).sum

//   val x = Jet(
//     Matrix.fromRows[Double](
//       Array(1.0, 2.0),
//       Array(3.0, 4.0)
//     )
//   ) + Jet.h[Matrix[Double]](0)

//   val y = Jet(
//     Matrix.fromRows(
//       Array(1.0, 2.0) * 2,
//       Array(3.0, 4.0) * 2
//     )
//   ) + Jet.h[Matrix[Double]](1)

//   println("MATRIX2 -------")
//   val out = gVecMat(x, y)

//   println("input: x ")

//   println(x.real.printMat)
//   println(x.infinitesimal.foreach(_.printMat))

//   println("input: y ")

//   println(y.real.printMat)
//   println(y.infinitesimal.foreach(_.printMat))

//   println("Result of gVecMat:")
//   println(out.real.printMat)
//   println("dim 1")
//   println(out.infinitesimal(0).printMat)
//   println("dim 2")
//   println(out.infinitesimal(1).printMat)
//   println
