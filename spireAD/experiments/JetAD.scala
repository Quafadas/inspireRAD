/*
 * **********************************************************************\
 * * Project                                                              **
 * *       ______  ______   __    ______    ____                          **
 * *      / ____/ / __  /  / /   / __  /   / __/     (c) 2011-2021        **
 * *     / /__   / /_/ /  / /   / /_/ /   / /_                            **
 * *    /___  / / ____/  / /   / __  /   / __/   Erik Osheim, Tom Switzer **
 * *   ____/ / / /      / /   / / | |   / /__                             **
 * *  /_____/ /_/      /_/   /_/  |_|  /____/     All rights reserved.    **
 * *                                                                      **
 * *      Redistribution and use permitted under the MIT license.         **
 * *                                                                      **
 * \***********************************************************************
 */

import scala.math._
import scala.reflect._
import spire.algebra._
import spire.std.ArraySupport
import spire.syntax.isReal._
import spire.syntax.nroot._
import spire.syntax.vectorSpace._

import vecxt.all.*
import vecxt.RowCol
import vecxt.BoundsCheck.DoBoundsCheck.no
import spire.math.JetDim

/** Used to implicitly define the dimensionality of the JetAD space.
  * @param dimension
  *   the number of dimensions.
  */
case class JetADDim(rc: RowCol) {
  require(rc._1 > 0 && rc._2 > 0)
  def size = rc._1 * rc._2

}

// scalastyle:off regex
/** ==Overview==
  * A simple implementation of N-dimensional dual numbers, for automatically
  * computing exact derivatives of functions. This code (and documentation)
  * closely follow the one in Google's "Ceres" library of non-linear
  * least-squares solvers (see <a
  * href="http://code.google.com/p/ceres-solver">Sameer Agarwal, Keir Mierle,
  * and others: Ceres Solver.</a>)
  *
  * While a complete treatment of the mechanics of automatic differentiation is
  * beyond the scope of this header (see
  * http://en.wikipedia.org/wiki/Automatic_differentiation for details), the
  * basic idea is to extend normal arithmetic with an extra element "h" such
  * that h != 0, but h^2^ = 0. Dual numbers are extensions of the real numbers
  * analogous to complex numbers: whereas complex numbers augment the reals by
  * introducing an imaginary unit i such that i^2^ = -1, dual numbers introduce
  * an "infinitesimal" unit h such that h^2^ = 0. Analogously to a complex
  * number c = x + y*i, a dual number d = x * y*h has two components: the "real"
  * component x, and an "infinitesimal" component y. Surprisingly, this leads to
  * a convenient method for computing exact derivatives without needing to
  * manipulate complicated symbolic expressions.
  *
  * For example, consider the function
  * {{{
  *   f(x) = x * x ,
  * }}}
  * evaluated at 10. Using normal arithmetic,
  * {{{
  * f(10) = 100, and df/dx(10) = 20.
  * }}}
  * Next, augment 10 with an infinitesimal h to get:
  * {{{
  *   f(10 + h) = (10 + h) * (10 + h)
  *             = 100 + 2 * 10 * h + h * h
  *             = 100 + 20 * h       +---
  *                     +-----       |
  *                     |            +--- This is zero
  *                     |
  *                     +----------------- This is df/dx
  * }}}
  * Note that the derivative of f with respect to x is simply the infinitesimal
  * component of the value of f(x + h). So, in order to take the derivative of
  * any function, it is only necessary to replace the numeric "object" used in
  * the function with one extended with infinitesimals. The class JetAD, defined
  * in this header, is one such example of this, where substitution is done with
  * generics.
  *
  * To handle derivatives of functions taking multiple arguments, different
  * infinitesimals are used, one for each variable to take the derivative of.
  * For example, consider a scalar function of two scalar parameters x and y:
  * {{{
  *   f(x, y) = x * x + x * y
  * }}}
  * Following the technique above, to compute the derivatives df/dx and df/dy
  * for f(1, 3) involves doing two evaluations of f, the first time replacing x
  * with x + h, the second time replacing y with y + h.
  *
  * For df/dx:
  * {{{
  *   f(1 + h, y) = (1 + h) * (1 + h) + (1 + h) * 3
  *               = 1 + 2 * h + 3 + 3 * h
  *               = 4 + 5 * h
  *
  *   Therefore df/dx = 5
  * }}}
  * For df/dy:
  * {{{
  *   f(1, 3 + h) = 1 * 1 + 1 * (3 + h)
  *               = 1 + 3 + h
  *               = 4 + h
  *
  *   Therefore df/dy = 1
  * }}}
  * To take the gradient of f with the implementation of dual numbers ("JetADs")
  * in this file, it is necessary to create a single JetAD type which has
  * components for the derivative in x and y, and pass them to a routine
  * computing function f. It is convenient to use a generic version of f, that
  * can be called also with non-JetAD numbers for standard evaluation:
  * {{{
  *   def f[@specialized(Double) T : Field](x: T, y: T): T = x * x + x * y
  *
  *   val xValue = 9.47892774
  *   val yValue = 0.287740
  *
  *   // The "2" means there should be 2 dual number components.
  *   implicit val dimension = JetADDim(2)
  *   val x: JetAD[Double] = xValue + JetAD.h[Double](0);  // Pick the 0th dual number for x.
  *   val y: JetAD[Double] = yValue + JetAD.h[Double](1);  // Pick the 1th dual number for y.
  *
  *   val z: JetAD[Double] = f(x, y);
  *   println("df/dx = " + z.infinitesimal(0) + ", df/dy = " + z.infinitesimal(1));
  * }}}
  *
  * For the more mathematically inclined, this file implements first-order
  * "JetADs". A 1st order JetAD is an element of the ring
  * {{{
  *   T[N] = T[t_1, ..., t_N] / (t_1, ..., t_N)^2
  * }}}
  * which essentially means that each JetAD consists of a "scalar" value 'a'
  * from T and a 1st order perturbation vector 'v' of length N:
  * {{{
  *   x = a + \sum_i v[i] t_i
  * }}}
  * A shorthand is to write an element as x = a + u, where u is the
  * perturbation. Then, the main point about the arithmetic of JetADs is that
  * the product of perturbations is zero:
  * {{{
  *   (a + u) * (b + v) = ab + av + bu + uv
  *                     = ab + (av + bu) + 0
  * }}}
  * which is what operator* implements below. Addition is simpler:
  * {{{
  *   (a + u) + (b + v) = (a + b) + (u + v).
  * }}}
  * The only remaining question is how to evaluate the function of a JetAD, for
  * which we use the chain rule:
  * {{{
  *   f(a + u) = f(a) + f'(a) u
  * }}}
  * where f'(a) is the (scalar) derivative of f at a.
  *
  * By pushing these things through generics, we can write routines that at same
  * time evaluate mathematical functions and compute their derivatives through
  * automatic differentiation.
  */
object JetAD extends JetADInstances {
  // No-arg c.tor makes a zero JetAD
  def apply(implicit
      c: ClassTag[Array[Double]],
      d: JetADDim,
      s: Semiring[Array[Double]]
  ): JetAD = JetAD(s.zero)

  // From real.
  def apply(
      real: Array[Double]
  )(implicit
      d: JetADDim
  ): JetAD =
    new JetAD(
      real,
      Matrix(Array.fill[Double](d.size)(0.0), d.rc)
    )

  // From real, to compute k-th partial derivative.
  def apply(a: Array[Double], k: Int)(implicit
      d: JetADDim
  ): JetAD = {
    val v = Matrix(Array.fill[Double](d.size)(0.0), d.rc)
    v((k, k)) = 1.0
    new JetAD(a, v)
  }

  // Zero real, indicator for k-th partial derivative.
  def h(
      k: Int
  )(implicit
      c: ClassTag[Array[Double]],
      d: JetADDim,
      r: Rig[Array[Double]]
  ): JetAD =
    JetAD(r.zero, k)

  def one(implicit
      c: ClassTag[Array[Double]],
      d: JetADDim,
      r: Rig[Array[Double]]
  ): JetAD = JetAD(r.one)

  def zero(implicit
      c: ClassTag[Double],
      d: JetADDim,
      s: Semiring[Array[Double]]
  ): JetAD = JetAD(s.zero)

  def fromInt(
      n: Int
  )(implicit
      d: JetADDim
  ): JetAD =
    JetAD(Array.fill(d.size)(n.toDouble))
}

object JetADExt:
  extension (arr: Array[Double])
    def *:(m: Matrix[Double]): Matrix[Double] =
      val cols = for (i <- 0 until arr.length) yield m.col(i) * arr(i)
      Matrix.fromColumns(cols*)

  end extension

@SerialVersionUID(0L)
final case class JetAD(
    real: Array[Double],
    infinitesimal: Matrix[Double]
) { lhs =>

  import spire.syntax.order._
  extension (arr: Array[Double])
    def *:(m: Matrix[Double]): Matrix[Double] =
      val cols =
        for (i <- 0 until arr.length)
          yield m.col(i) * arr(i)

      Matrix.fromColumns(cols*)

  def dimension: RowCol = infinitesimal.shape
  implicit def JetADDimension: JetADDim = JetADDim(dimension)

  /** This is consistent with abs
    */
  // def signum(implicit r: Signed[Array[Double]]): Int = real.signum

  // def asTuple: (T, Array[Array[Double]]) = (real, infinitesimal)

  def isReal: Boolean = true
  def isZero: Boolean = anyIsZero(real) && isReal
  def isInfinitesimal: Boolean = anyIsZero(real) && !isReal

  def eqv(b: JetAD)(implicit o: Eq[Array[Double]]): Boolean = {
    real === b.real && ArraySupport.eqv(infinitesimal.raw, b.infinitesimal.raw)
  }
  def neqv(b: JetAD)(implicit o: Eq[Array[Double]]): Boolean = {
    !this.eqv(b)
  }

  def unary_-(implicit
      f: Field[Array[Double]],
      v: VectorSpace[Array[Double], Double]
  ): JetAD = {
    new JetAD(real * -1, infinitesimal * -1)
  }

  def +(b: Double)(implicit f: Field[Array[Double]]): JetAD =
    new JetAD(real + b, infinitesimal)
  def -(b: Double)(implicit f: Field[Array[Double]]): JetAD =
    new JetAD(real - b, infinitesimal)
  def *(b: Double)(implicit
      fAD: Field[Array[Double]],
      f: Field[Double],
      fm: Field[Matrix[Double]],
      v: VectorSpace[Array[Double], Double],
      jd: JetADDim
  ): JetAD = {
    new JetAD(real * b, b * infinitesimal)
  }
  def /(b: Double)(implicit
      fAD: Field[Array[Double]],
      f: Field[Double],
      fm: Field[Matrix[Double]],
      v: VectorSpace[Array[Double], Double],
      jd: JetADDim
  ): JetAD = {
    new JetAD(real / b, infinitesimal / b)
  }
  def +(
      b: JetAD
  )(implicit
      f: Field[Array[Double]],
      v: VectorSpace[Array[Double], Double]
  ): JetAD = {
    new JetAD(real + b.real, infinitesimal + b.infinitesimal)
  }
  def -(
      b: JetAD
  )(implicit
      fAD: Field[Array[Double]],
      f: Field[Double],
      v: VectorSpace[Array[Double], Double],
      jd: JetADDim
  ): JetAD = {
    new JetAD(real - b.real, infinitesimal - b.infinitesimal)
  }
  // Multiplication rule for differentials:
  //
  //    (a + du)(b + dv) ~= ab + a dv + b du
  //
  // because du dv ~= 0
  def *(
      b: JetAD
  )(implicit
      fAD: Field[Array[Double]],
      f: Field[Double],
      v: VectorSpace[Array[Double], Double]
  ): JetAD = {

    val mx1 = b.real *: infinitesimal
    val mx2 = real *: b.infinitesimal

    new JetAD(
      real * b.real,
      mx1 + mx2
    )
  }

  def /(
      b: JetAD
  )(implicit
      f: Field[Array[Double]],
      v: VectorSpace[Array[Double], Double]
  ): JetAD = {
    // Division rule for differentials:
    //
    //   a + du   (a + du)(b - dv)    ab - a dv + b du   a    a       1      a   1         a
    //   ------ = ---------------- ~= ---------------- = - - --- dv + - du = - + - * (du - - dv)
    //   b + dv   (b + dv)(b - dv)           b^2         b   b^2      b      b   b         b
    //
    // which holds because dv dv = du dv = 0.
    val br_inv = b.real.map(1.0 / _)
    val ar_div_br = real * br_inv
    new JetAD(
      ar_div_br,
      br_inv *: (infinitesimal - (ar_div_br *: b.infinitesimal))
    )
  }

  def /~(b: JetAD)(implicit
      c: ClassTag[Array[Double]],
      f: Field[Array[Double]],
      r: IsReal[Array[Double]],
      v: VectorSpace[Array[Double], Double]
  ): JetAD = {
    val q = this / b
    new JetAD(
      q.real.floor,
      ??? // be wary columnwise
      // q.infinitesimal.map(r.floor)
    )
  }

  def %(b: JetAD)(implicit
      c: ClassTag[Array[Double]],
      f: Field[Array[Double]],
      r: IsReal[Array[Double]],
      v: VectorSpace[Array[Double], Double]
  ): JetAD = {
    ???
    // this - ((this /~ b) * b)
  }

  // def /%(
  //     b: JetAD
  // )(implicit
  //     c: ClassTag[Array[Double]],
  //     f: Field[Array[Double]],
  //     r: IsReal[Array[Double]],
  //     v: VectorSpace[Array[Double], Double]
  // ): (JetAD, JetAD) = {
  //   val q = this /~ b
  //   (q, this - (q * b))
  // }

  def **(b: Int)(implicit
      f: Field[Array[Double]],
      v: VectorSpace[Array[Double], Double]
  ): JetAD =
    pow(b)

  // def nroot(
  //     k: Int
  // )(implicit
  //     f: Field[Array[Double]],
  //     o: Order[Array[Double]],
  //     s: Signed[Array[Double]],
  //     t: Trig[Array[Double]],
  //     v: VectorSpace[Array[Double], Double]
  // ): JetAD = {
  //   pow(f.fromInt(k).reciprocal)
  // }

  // def **(
  //     b: JetAD
  // )(implicit
  //     c: ClassTag[Array[Double]],
  //     f: Field[Array[Double]],
  //     o: Order[Array[Double]],
  //     s: Signed[Array[Double]],
  //     t: Trig[Array[Double]],
  //     v: VectorSpace[Array[Double], Double]
  // ): JetAD = {
  //   pow(b)
  // }

  def floor(implicit
      r: IsReal[Double],
      jd: JetADDim
  ): JetAD = {
    new JetAD(
      real.map(r.floor),
      ??? // be wary - need to do the floor columnwise.
      // Matrix(infinitesimal.raw.map(r.floor), jd.rc)
    )
  }

  def ceil(implicit
      c: ClassTag[Array[Double]],
      r: IsReal[Array[Double]],
      jd: JetADDim
  ): JetAD = {
    new JetAD(
      real.ceil,
      ??? // be wary - need to do the floor columnwise.
      //  Matrixinfinitesimal.raw.map(r.ceil)
    )
  }

  def round(implicit
      c: ClassTag[Array[Double]],
      r: IsReal[Array[Double]]
  ): JetAD = {
    new JetAD(
      real.round,
      ???
      // infinitesimal.map(r.round)
    )
  }

  // Elementary math functions
  // In general, f(a + du) ~= f(a) + f'(a) du .

  /** abs(x + du) ~= x + du or -(x + du)
    */
  def abs(implicit
      f: Field[Array[Double]],
      o: Order[Array[Double]],
      s: Signed[Array[Double]],
      v: VectorSpace[Array[Double], Double]
  ): JetAD = {
    ???
    // if (real < f.zero) new JetAD(-real, -infinitesimal)
    // else this
  }

  // spire.math. does not define this pow generically, so there it is
  // private def powScalarToScalar(
  //     b: T,
  //     e: T
  // )(implicit
  //     f: Field[Array[Double]],
  //     o: Order[Array[Double]],
  //     s: Signed[Array[Double]],
  //     t: Trig[Array[Double]]
  // ): T = {
  //   if (e === f.zero) {
  //     one
  //   } else if (b === f.zero) {
  //     if (e < f.zero) throw new Exception("raising 0 to a negative power")
  //     else f.zero
  //   } else {
  //     spire.math.exp(e * spire.math.log(b))
  //   }
  // }

  // pow -- base is a constant, exponent (this) is a differentiable function.
  // b^(p + du) ~= b^p + b^p * log(b) du
  // def powScalarToJetAD(
  //     a: T
  // )(implicit
  //     c: ClassTag[Array[Double]],
  //     f: Field[Array[Double]],
  //     m: CModule[Array[Array[Double]], T],
  //     o: Order[Array[Double]],
  //     s: Signed[Array[Double]],
  //     t: Trig[Array[Double]]
  // ): JetAD = {
  //   if (isZero) {
  //     JetAD(Array.fill(jd.size)(1.0))
  //   } else {
  //     val tmp = powScalarToScalar(a, real)
  //     new JetAD(tmp, (spire.math.log(a) * tmp) *: infinitesimal)
  //   }
  // }

  /** pow -- base (this) is a differentiable function, exponent is a constant.
    * {{{
    * pow(a + du, p) ~= pow(a, p) + p * pow(a, p-1) du
    * }}}
    */
  // def pow(p: T)(implicit
  //     f: Field[Array[Double]],
  //     o: Order[Array[Double]],
  //     s: Signed[Array[Double]],
  //     t: Trig[Array[Double]],
  //     v: VectorSpace[Array[Double], Double]
  // ): JetAD = {
  //   val tmp: T = p * powScalarToScalar(real, p - one)
  //   new JetAD(powScalarToScalar(real, p), tmp *: infinitesimal)
  // }

  // As above, integer exponent.
  def pow(
      p: Int
  )(implicit
      f: Field[Array[Double]],
      v: VectorSpace[Array[Double], Double]
  ): JetAD = {
    val tmp = p * f.pow(real, p - 1)
    new JetAD(f.pow(real, p), tmp *: infinitesimal)
  }

  /** pow -- both base (this) and exponent are differentiable functions.
    * {{{
    * (a + du)^(b + dv) ~= a^b + b * a^(b-1) du + a^b log(a) dv
    * }}}
    */
  // def pow(
  //     b: JetAD
  // )(implicit
  //     c: ClassTag[Array[Double]],
  //     f: Field[Array[Double]],
  //     m: CModule[Array[Array[Double]], Double],
  //     o: Order[Array[Double]],
  //     s: Signed[Array[Double]],
  //     t: Trig[Array[Double]],
  //     jd: JetADDim
  // ): JetAD = {
  //   if (b.isZero) {
  //     JetAD(Array.fill(jd.size)(1.0))
  //   } else {
  //     val tmp1 = powScalarToScalar(real, b.real)
  //     val tmp2 = b.real * powScalarToScalar(real, b.real - one)
  //     val tmp3 = tmp1 * spire.math.log(real)
  //     new JetAD(tmp1, (tmp2 *: infinitesimal) + (tmp3 *: b.infinitesimal))
  //   }
  // }

  /** log(a + du) ~= log(a) + du / a
    */
  def log(implicit
      f: Field[Array[Double]],
      t: Trig[Array[Double]],
      v: VectorSpace[Array[Double], Double]
  ): JetAD = {
    new JetAD(
      spire.math.log(real),
      (Array.fill[Double](real.length)(1.0) / real) *: infinitesimal
    )
  }

  def one = Array.fill[Double](real.length)(1.0)

  /** sqrt(a + du) ~= sqrt(a) + du / (2 sqrt(a))
    */
  def sqrt(implicit
      f: Field[Array[Double]],
      n: NRoot[Array[Double]],
      v: VectorSpace[Array[Double], Double]
  ): JetAD = {
    val sa = real.sqrt

    val oneHalf = one / (one * 2)
    new JetAD(sa, (oneHalf / sa) *: infinitesimal)
  }

  /** acos(a + du) ~= acos(a) - 1 / sqrt(1 - a**2) du
    */
  def acos(implicit
      f: Field[Array[Double]],
      n: NRoot[Array[Double]],
      t: Trig[Array[Double]],
      v: VectorSpace[Array[Double], Double]
  ): JetAD = {
    val tmp = -1.0 / spire.math.sqrt(one - real * real)
    new JetAD(spire.math.acos(real), tmp *: infinitesimal)
  }

  /** asin(a + du) ~= asin(a) - 1 / sqrt(1 - a**2) du
    */
  def asin(implicit
      f: Field[Array[Double]],
      n: NRoot[Array[Double]],
      t: Trig[Array[Double]],
      v: VectorSpace[Array[Double], Double]
  ): JetAD = {
    val tmp = one / spire.math.sqrt(one - real * real)
    new JetAD(spire.math.asin(real), tmp *: infinitesimal)
  }

  /** atan(a + du) ~= atan(a) + 1 / (1 + a**2) du
    */
  def atan(implicit
      f: Field[Array[Double]],
      t: Trig[Array[Double]],
      v: VectorSpace[Array[Double], Double]
  ): JetAD = {
    val tmp = one / (one + real * real)
    new JetAD(spire.math.atan(real), tmp *: infinitesimal)
  }

  /** Defined with "this" as the y coordinate:
    * {{{
    * this.atan2(a) == atan2(this, a) == atan(this / a) atan2(b + dv, a + du) ~= atan2(b, a) + (- b du + a dv) / (a^2 + b^2)
    * }}}
    */
  def atan2(
      a: JetAD
  )(implicit
      f: Field[Array[Double]],
      t: Trig[Array[Double]],
      v: VectorSpace[Array[Double], Double]
  ): JetAD = {
    val tmp = one / (a.real * a.real + real * real)
    new JetAD(
      spire.math.atan2(real, a.real),
      ((tmp * (real * -1)) *: a.infinitesimal) + ((tmp * a.real) *: infinitesimal)
    )
  }

  /** exp(a + du) ~= exp(a) + exp(a) du
    */
  def exp(implicit
      t: Trig[Array[Double]],
      v: VectorSpace[Array[Double], Double]
  ): JetAD = {
    val ea = real.exp
    new JetAD(ea, ea *: infinitesimal)
  }

  /** sin(a + du) ~= sin(a) + cos(a) du
    */
  def sin(implicit
      t: Trig[Array[Double]],
      v: VectorSpace[Array[Double], Double]
  ): JetAD = {
    new JetAD(spire.math.sin(real), spire.math.cos(real) *: infinitesimal)
  }

  /** sinh(a + du) ~= sinh(a) + cosh(a) du
    */
  def sinh(implicit
      t: Trig[Array[Double]],
      v: VectorSpace[Array[Double], Double]
  ): JetAD = {
    new JetAD(spire.math.sinh(real), spire.math.cosh(real) *: infinitesimal)
  }

  /** cos(a + du) ~= cos(a) - sin(a) du
    */
  def cos(implicit
      f: Field[Array[Double]],
      t: Trig[Array[Double]],
      v: VectorSpace[Array[Double], Double]
  ): JetAD =
    new JetAD(
      spire.math.cos(real),
      spire.math.sin(real) *: (infinitesimal * -1)
    )

  /** cosh(a + du) ~= cosh(a) + sinh(a) du
    */
  def cosh(implicit
      t: Trig[Array[Double]],
      v: VectorSpace[Array[Double], Double]
  ): JetAD = {
    new JetAD(spire.math.cosh(real), spire.math.sinh(real) *: infinitesimal)
  }

  /** tan(a + du) ~= tan(a) + (1 + tan(a)**2) du
    */
  def tan(implicit
      f: Field[Array[Double]],
      t: Trig[Array[Double]],
      v: VectorSpace[Array[Double], Double]
  ): JetAD = {
    val tan_a = spire.math.tan(real)
    val tmp = one + tan_a * tan_a
    new JetAD(tan_a, tmp *: infinitesimal)
  }

  /** tanh(a + du) ~= tanh(a) + (1 - tanh(a)**2) du
    */
  def tanh(implicit
      f: Field[Array[Double]],
      t: Trig[Array[Double]],
      v: VectorSpace[Array[Double], Double]
  ): JetAD = {
    val tanh_a = spire.math.tanh(real)
    val tmp = one - tanh_a * tanh_a
    new JetAD(tanh_a, tmp *: infinitesimal)
  }

  // Object stuff
  override def hashCode: Int = {
    if (isReal) real.##
    else 13 * real.## + infinitesimal.raw.foldLeft(53)((x, y) => x + y.## * 19)
  }

  override def equals(that: Any): Boolean = that match {
    case that: JetAD => this === that
    case that        => isReal && real == that
  }

  def ===(that: JetAD): Boolean =
    real == that.real && dimension == that.dimension &&
      infinitesimal.raw.zip(that.infinitesimal.raw).forall { case (x, y) =>
        x == y
      }

  def =!=(that: JetAD): Boolean =
    !(this === that)

  override def toString: String = {
    s"""Real: ${real.mkString(
        "[",
        ",",
        "]"
      )}\nPartials:\n${infinitesimal.printMat}"""
  }
}

trait JetADInstances {
  implicit def JetADAlgebra(implicit
      c: ClassTag[Array[Double]],
      d: JetADDim,
      fd: Field[Double],
      o: Order[Array[Double]],
      s: Signed[Array[Double]],
      e: Eq[Array[Double]]
  ): JetADAlgebra = {
    import spire.std.array.ArrayVectorSpace
    // import spire.implicits.DoubleAlgebra
    import JetArrayTypeClasses.nrootAD
    import JetArrayTypeClasses.fadAD
    import JetArrayTypeClasses.trigAD

    new JetADAlgebra
  }
}

trait JetADIsRing extends Ring[JetAD] {
  implicit def c: ClassTag[Double]
  implicit def d: JetADDim
  implicit def eq: Eq[Array[Double]]
  implicit def fd: Field[Double]
  implicit def fad: Field[Array[Double]]
  implicit def s: Signed[Array[Double]]
  implicit def t: Trig[Array[Double]]
  implicit def v: VectorSpace[Array[Double], Double]
  implicit def dr: Field[Double]
  import spire.implicits.DoubleAlgebra
  import JetArrayTypeClasses.fadAD
  import JetArrayTypeClasses.arrAD

  override def minus(
      a: JetAD,
      b: JetAD
  ): JetAD =
    a - b

  def negate(a: JetAD): JetAD = -a
  def one: JetAD = ???
  def plus(
      a: JetAD,
      b: JetAD
  ): JetAD = a + b
  override def pow(a: JetAD, b: Int): JetAD =
    a.pow(b)
  override def times(
      a: JetAD,
      b: JetAD
  ): JetAD =
    println("a.real: " + a.real)
    a * b
  def zero: JetAD = ???

  override def fromInt(n: Int): JetAD =
    ???
    // JetAD.fromInt[Array[Double]](n)
}

/* TODO: JetAD is probably not a genuine GCD ring */
trait JetADIsGCDRing extends JetADIsRing with GCDRing[JetAD] {
  /* TODO: What exactly is this GCD trying to achieve? Tests? */
  /*def gcd(a: JetAD, b: JetAD): JetAD = {
    @tailrec def _gcd(a: JetAD, b: JetAD): JetAD =
      if (b.isZero) a else _gcd(b, a - (a / b).round * b)
    _gcd(a, b)
  }*/
}

/* TODO: JetAD is probably not a genuine Euclidean ring */
trait JetADIsEuclideanRing extends JetADIsGCDRing with EuclideanRing[JetAD] {
  /*def euclideanFunction(a: JetAD): BigInt = sys.error("Clarify JetAD first, see #598")
  /* TODO: what are exactly the laws of JetAD with respect to EuclideanRing ? */
  def quot(a: JetAD, b: JetAD): JetAD = a /~ b
  def mod(a: JetAD, b: JetAD): JetAD = a % b
  override def quotmod(a: JetAD, b: JetAD): (JetAD, JetAD) = a /% b*/
}

/* TODO: JetAD is probably not a genuine Field */
trait JetADIsField(using
    td: JetADDim
) extends JetADIsEuclideanRing
    with Field[JetAD] {

  import JetArrayTypeClasses.arrAD

  override def fromDouble(n: Double): JetAD = JetAD(
    Array.fill(td.size)(n)
  )
  def div(
      a: JetAD,
      b: JetAD
  ): JetAD = a / b
}

trait JetADIsTrig extends Trig[JetAD] {
  implicit def cad: ClassTag[Array[Double]]
  implicit def c: ClassTag[Double]
  implicit def d: JetADDim
  implicit def fd: Field[Double]
  implicit def n: NRoot[Array[Double]]
  implicit def s: Signed[Array[Double]]
  implicit def t: Trig[Array[Double]]
  implicit def v: VectorSpace[Array[Double], Double]

  import JetArrayTypeClasses.fadAD
  def e: JetAD = JetAD(t.e)
  def pi: JetAD = JetAD(t.pi)

  def exp(a: JetAD): JetAD = a.exp
  def expm1(a: JetAD): JetAD = ???
  def log(a: JetAD): JetAD = a.log
  def log1p(a: JetAD): JetAD = ???

  def sin(a: JetAD): JetAD = a.sin
  def cos(a: JetAD): JetAD = a.cos
  def tan(a: JetAD): JetAD = a.tan

  def asin(a: JetAD): JetAD = a.asin
  def acos(a: JetAD): JetAD = a.acos
  def atan(a: JetAD): JetAD = a.atan
  def atan2(
      y: JetAD,
      x: JetAD
  ): JetAD = y.atan2(x)

  def sinh(x: JetAD): JetAD = x.sinh
  def cosh(x: JetAD): JetAD = x.cosh
  def tanh(x: JetAD): JetAD = x.tanh

  def toRadians(a: JetAD): JetAD = a
  def toDegrees(a: JetAD): JetAD = a
}

trait JetADIsNRoot extends NRoot[JetAD] {
  implicit def fd: Field[Double]
  implicit def fad: Field[Array[Double]]
  implicit def n: NRoot[Array[Double]]
  implicit def o: Order[Array[Double]]
  implicit def t: Trig[Array[Double]]
  implicit def s: Signed[Array[Double]]
  implicit def c: ClassTag[Double]
  implicit def v: VectorSpace[Array[Double], Double]

  def nroot(a: JetAD, k: Int): JetAD = ??? // a.nroot(k)
  override def sqrt(a: JetAD): JetAD = ??? // a.sqrt
  def fpow(
      a: JetAD,
      b: JetAD
  ): JetAD =
    ???
    // a.pow(b)
  def fpow(a: Array[Double], b: JetAD): JetAD =
    ???
    // b.powScalarToJetAD(a)
}

@SerialVersionUID(0L)
class JetADEq[T: Eq] extends Eq[JetAD] with Serializable {
  import spire.implicits.ArrayEq

  def eqv(x: JetAD, y: JetAD): Boolean = x.eqv(y)
  override def neqv(x: JetAD, y: JetAD): Boolean =
    x.neqv(y)
}

/* TODO
trait JetADIsSigned[Array[Double]] extends Signed[JetAD] {
  implicit def f: Field[Array[Double]]
  implicit def r: IsReal[Array[Double]]
  implicit def t: Trig[Array[Double]]
  implicit def v: VectorSpace[Array[Array[Double]],T]

  def signum(a: JetAD): Int = a.signum
  def abs(a: JetAD): JetAD = a.abs
  def compare(x: JetAD, y: JetAD): Int = ???
}
 */

@SerialVersionUID(0L)
class JetADAlgebra(implicit
    val cad: ClassTag[Array[Double]],
    val c: ClassTag[Double],
    val d: JetADDim,
    val eq: Eq[Array[Double]],
    val fd: Field[Double],
    val fad: Field[Array[Double]],
    val n: NRoot[Array[Double]],
    val o: Order[Array[Double]],
    val t: Trig[Array[Double]],
    val s: Signed[Array[Double]],
    val v: VectorSpace[Array[Double], Double]
) extends JetADIsField
    with JetADIsTrig
    with JetADIsNRoot
//  with JetADIsSigned[Array[Double]]
    with VectorSpace[JetAD, Double]
    with FieldAssociativeAlgebra[JetAD, Double]
    with Serializable {

  implicit def dr: Field[Double] = fd

  def scalar: Field[Double] = fd
  def nroot: NRoot[Array[Double]] = n
  def timesl(r: Double, w: JetAD): JetAD = ??? // JetAD(a) * w
  def dot(x: JetAD, y: JetAD): JetAD = {
    ???
    // x.infinitesimal
    //   .zip(y.infinitesimal)
    //   .foldLeft { scalar.times(x.real, y.real) } { (xx, yy) =>
    //     scalar.plus(xx, scalar.times(yy._1, yy._2))
    //   }
  }
}
