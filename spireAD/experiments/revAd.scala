// import spire._
// import spire.math._
// import spire.implicits._
// import _root_.algebra.ring.Field
// import spire.algebra.Trig
// import spire.algebra.VectorSpace

// @main def revAd =

//   given jd: TejDim = TejDim(2)
//   val x: Tej[Double] = 2.0 + Tej.h[Double](0)
//   val y: Tej[Double] = 3.0 + Tej.h[Double](1)

//   def f[@specialized(Double) T: Field](x: T, y: T): T = x * x + x * y

//   // https://stats.stackexchange.com/questions/224140/step-by-step-example-of-reverse-mode-automatic-differentiation
//   def so[F[_], @specialized(Double) T: Field: Trig](x: T, y: T): T =
//     x * y + sin(x)

//   def soV[F[_], @specialized(Double) T: Field: Trig](
//       x: T,
//       y: T
//   )(using r: Reducible[F, T]): T = x * y + sin(x)

//   def g[@specialized(Double) T: Field: Trig](x: T, y: T): T =
//     sin(log(exp(x) + exp(-y)) - x / cos(y))

//   val z = so(x, y)

//   println(sin(2.0))

//   // println(f(x, y))
//   println(z)
//   println(jd.dag.toGraphviz)
