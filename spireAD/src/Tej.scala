package io.github.quafadas.spireAD

import scala.math.*
import scala.reflect.*

import spire.algebra.*
import spire.syntax.isReal.*
import spire.syntax.vectorSpace.*
import scala.util.chaining.*
import scala.specialized as sp

enum AdMode:
  case Forward, Reverse
end AdMode

/** Used to implicitly define the dimensionality of the Tej space.
  * @param dimension
  *   the number of dimensions.
  */
case class TejDim[T: Field: ClassTag: Trig: NRoot]():

  val dag = DAG[T]()
  final val mode = AdMode.Forward

  inline def addToGraph(t: Tej[T]): Tej[T] = t.tap(dag.addTejNode)
  inline def unary(input: Tej[T], op: TejOpUrnary[T]): Tej[T] =
    val tn = TejNode(input)
    dag.addNode(op)
    dag.addEdge(tn, op)
    op.value
  end unary

  inline def binary(
      lhs: Tej[T],
      rhs: Tej[T],
      op: TejOpBinary[T]
  ): Tej[T] =
    val l = TejNode(lhs)
    val r = TejNode(rhs)
    dag.addNode(op)
    dag.addEdge(l, op)
    dag.addEdge(r, op)
    op.value
  end binary
end TejDim

object Tej extends TejInstances:
  // No-arg c.tor makes a zero Tej
  inline def apply[@sp(Float, Double) T](implicit
      c: ClassTag[T],
      inline d: TejDim[T],
      s: Semiring[T],
      r: Ring[T]
  ): Tej[T] =
    d.addToGraph(Tej[T](tejNum = s.zero))

  // From real.
  inline def apply[@sp(Float, Double) T](
      real: T
  )(implicit
      c: ClassTag[T],
      inline d: TejDim[T]
  ): Tej[T] =
    d.addToGraph(Tej[T](tejNum = real))

  // // From real, to compute k-th partial derivative.
  // inline def apply[@sp(Float, Double) T](a: T, k: Int)(implicit
  //     c: ClassTag[T],
  //     inline d: TejDim[T],
  //     r: Rig[T],
  //     s: Semiring[T]
  // ): Tej[T] =
  //   val v = Array.fill[T](d.dimension)(r.zero)
  //   v(k) = r.one
  //   d.addToGraph(Tej(Jet(a, v)))
  // end apply

  // // From real, to compute k-th partial derivative.
  // inline def apply[@sp(Float, Double) T](a: T, k: Array[T])(implicit
  //     // c: ClassTag[T],
  //     inline d: TejDim[T],
  //     r: Rig[T]
  // ): Tej[T] =
  //   d.addToGraph(Tej(Jet(a, k)))

  // Zero real, indicator for k-th partial derivative.
  // inline def h[@sp(Float, Double) T](
  //     k: Int
  // )(implicit
  //     c: ClassTag[T],
  //     inline d: TejDim[T],
  //     r: Rig[T]
  // ): Tej[T] =
  //   d.addToGraph(Tej(r.zero, k))

  inline def one[@sp(Float, Double) T](implicit
      c: ClassTag[T],
      inline d: TejDim[T],
      r: Ring[T]
  ): Tej[T] =
    d.addToGraph(Tej[T](tejNum = r.one))

  def zero[@sp(Float, Double) T](implicit
      c: ClassTag[T],
      d: TejDim[T],
      s: Ring[T]
  ): Tej[T] =
    d.addToGraph(Tej[T](tejNum = s.zero))

  def fromInt[@sp(Float, Double) T](
      n: Int
  )(implicit
      c: ClassTag[T],
      d: TejDim[T],
      r: Ring[T]
  ): Tej[T] =
    d.addToGraph(
      Tej[T](tejNum = r.fromInt(n))
    )

  implicit def intToTej(n: Int)(implicit d: TejDim[Double]): Tej[Double] =
    doubleToTej(n.toDouble)

  implicit def longToTej(n: Long)(implicit d: TejDim[Double]): Tej[Double] =
    doubleToTej(n.toDouble)

  implicit def floatToTej[T](
      n: Float
  )(implicit d: TejDim[Float]): Tej[Float] =
    Tej(
      tejNum = n.toFloat
    )
  end floatToTej

  implicit def doubleToTej(
      n: Double
  )(implicit d: TejDim[Double]): Tej[Double] =
    d.addToGraph(Tej(tejNum = n))

  end doubleToTej

  implicit def bigIntToTej(
      n: BigInt
  )(implicit d: TejDim[BigDecimal]): Tej[BigDecimal] =
    bigDecimalToTej(BigDecimal(n))

  implicit def bigDecimalToTej(
      n: BigDecimal
  )(implicit d: TejDim[BigDecimal]): Tej[BigDecimal] =

    Tej(tejNum = n)
  end bigDecimalToTej
end Tej

@SerialVersionUID(0L)
case class Tej[@sp(Float, Double) T] private (tejNum: T)(using
    td: TejDim[T]
) extends ScalaNumber
    with ScalaNumericConversions
    with Serializable:
  lhs =>

  import spire.syntax.order.*

  // def dimension: Int = j.infinitesimal.size
  // implicit def TejDimension: TejDim = TejDim(dimension)
  // given jd: JetDim = td.jd
  // given ta: JetAlgebra[T] = ???

  final lazy val nodeId = java.util.UUID.randomUUID()

  // inline private def real = j.real
  // inline private def infinitesimal = j.infinitesimal

  def backward(using td: TejDim[T], f: Field[T]) =
    val sorted = td.dag.toposort
    sorted.last.grad = f.one

    // println(sorted.last)

    // println(
    //   "toposorted\n Start backwards prop \n" + sorted.reverse.mkString("\n")
    // )
    for node <- sorted.reverse do td.dag.getNode(node.id).backward
    end for

    sorted.reverse
  end backward

  /** This is consistent with abs
    */
  def signum(implicit r: Signed[T]): Int = tejNum.signum

  def isZero: Boolean = tejNum == 0

  def eqv(b: Tej[T])(implicit o: Eq[T]): Boolean =
    tejNum === b.tejNum

  def neqv(b: Tej[T])(implicit o: Eq[T]): Boolean =
    !this.eqv(b)

  def unary_-(implicit
      f: Field[T],
      t: Trig[T],
      n: NRoot[T],
      v: VectorSpace[Array[T], T],
      d: TejDim[T]
  ): Tej[T] =
    d.unary(
      this,
      TejOpUrnary(UrnaryOps.Neg, Tej(tejNum = -tejNum), this.nodeId)
    )
  end unary_-

  def +(b: T)(implicit f: Field[T], d: TejDim[T], ct: ClassTag[T], v: VectorSpace[Array[T], T]): Tej[T] =
    val tmp = Tej(tejNum = b)
    d.binary(
      this,
      tmp,
      TejOpBinary(BinaryOps.Add, tmp * this, this.nodeId, tmp.nodeId)
    )
  end +

  def -(b: T)(implicit f: Field[T]): Tej[T] =
    Tej(tejNum = tejNum - b)
  def *(
      b: T
  )(implicit
      f: Field[T],
      v: VectorSpace[Array[T], T],
      d: TejDim[T],
      ct: ClassTag[T]
  ): Tej[T] =
    val tmp = Tej(tejNum = b)
    d.binary(
      this,
      tmp,
      TejOpBinary(BinaryOps.Mul, tmp * this, this.nodeId, tmp.nodeId)
    )
  end *

  def /(b: T)(implicit
      f: Field[T],
      v: VectorSpace[Array[T], T]
  ): Tej[T] =
    Tej(tejNum = tejNum / b)

  def +(
      b: Tej[T]
  )(implicit
      f: Field[T],
      v: VectorSpace[Array[T], T],
      d: TejDim[T]
  ): Tej[T] =
    d.binary(
      this,
      b,
      TejOpBinary(BinaryOps.Add, Tej[T](tejNum = tejNum + b.tejNum), this.nodeId, b.nodeId)
    )

  def -(
      b: Tej[T]
  )(implicit
      f: Field[T],
      v: VectorSpace[Array[T], T],
      d: TejDim[T]
  ): Tej[T] =
    d.binary(
      this,
      b,
      TejOpBinary(BinaryOps.Sub, Tej[T](tejNum = tejNum - b.tejNum), this.nodeId, b.nodeId)
    )
  // Multiplication rule for differentials:
  //
  //    (a + du)(b + dv) ~= ab + a dv + b du
  //
  // because du dv ~= 0
  def *(
      b: Tej[T]
  )(implicit
      f: Field[T],
      v: VectorSpace[Array[T], T],
      d: TejDim[T]
  ): Tej[T] =
    d.binary(
      this,
      b,
      TejOpBinary(BinaryOps.Mul, Tej[T](tejNum = tejNum * b.tejNum), this.nodeId, b.nodeId)
    )

  def /(
      b: Tej[T]
  )(implicit
      f: Field[T],
      v: VectorSpace[Array[T], T],
      d: TejDim[T]
  ): Tej[T] =
    // Division rule for differentials:
    //
    //   a + du   (a + du)(b - dv)    ab - a dv + b du   a    a       1      a   1         a
    //   ------ = ---------------- ~= ---------------- = - - --- dv + - du = - + - * (du - - dv)
    //   b + dv   (b + dv)(b - dv)           b^2         b   b^2      b      b   b         b
    //
    // which holds because dv dv = du dv = 0.
    d.binary(
      this,
      b,
      TejOpBinary(BinaryOps.Div, Tej[T](tejNum = tejNum / b.tejNum), this.nodeId, b.nodeId)
    )

  def /~(b: Tej[T])(implicit
      c: ClassTag[T],
      f: Field[T],
      r: IsReal[T],
      v: VectorSpace[Array[T], T],
      d: TejDim[T] // Added Numeric[T] to implicits
  ): Tej[T] =
    ???
    // d.binary(this, b, TejOpBinary("/~", Tej(j /~ b.j), this.nodeId, b.nodeId))

  def %(b: Tej[T])(implicit
      c: ClassTag[T],
      f: Field[T],
      r: IsReal[T],
      v: VectorSpace[Array[T], T] // Added Numeric[T] to implicits
  ): Tej[T] =
    ???
    // Tej(j % b.j)

  def /%(
      b: Tej[T]
  )(implicit
      c: ClassTag[T],
      f: Field[T],
      r: IsReal[T],
      v: VectorSpace[Array[T], T] // Added Numeric[T] to implicits
  ): (Tej[T], Tej[T]) =
    ???
    // val q = this /~ b
    // (q, this - (q * b))

  def **(b: Int)(implicit f: Field[T], v: VectorSpace[Array[T], T]): Tej[T] =
    pow(b)

  def nroot(
      k: Int
  )(implicit
      f: Field[T],
      o: Order[T],
      s: Signed[T],
      t: Trig[T],
      v: VectorSpace[Array[T], T]
  ): Tej[T] =
    ???
    /// pow(f.fromInt(k).reciprocal)

  def **(
      b: Tej[T]
  )(implicit
      c: ClassTag[T],
      f: Field[T],
      o: Order[T],
      s: Signed[T],
      t: Trig[T],
      v: VectorSpace[Array[T], T]
  ): Tej[T] =
    ???
    // pow(b)

  def floor(implicit c: ClassTag[T], r: IsReal[T]): Tej[T] =
    ???
    // Tej(j.floor)

  def ceil(implicit c: ClassTag[T], r: IsReal[T]): Tej[T] =
    ???
    // Tej(j.ceil)

  def round(implicit c: ClassTag[T], r: IsReal[T]): Tej[T] =
    ???
    // Tej(j.round)

  // Elementary math functions
  // In general, f(a + du) ~= f(a) + f'(a) du .

  /** abs(x + du) ~= x + du or -(x + du)
    */
  def abs(implicit
      f: Field[T],
      o: Order[T],
      t: Trig[T],
      s: Signed[T],
      n: NRoot[T],
      v: VectorSpace[Array[T], T],
      d: TejDim[T]
  ): Tej[T] =
    d.unary(this, TejOpUrnary(UrnaryOps.Abs, Tej(tejNum = tejNum.abs), this.nodeId))

  // pow -- base is a constant, exponent (this) is a differentiable function.
  // b^(p + du) ~= b^p + b^p * log(b) du
  def powScalarToTej(
      a: T
  )(implicit
      c: ClassTag[T],
      f: Field[T],
      m: CModule[Array[T], T],
      o: Order[T],
      s: Signed[T],
      t: Trig[T],
      d: TejDim[T]
  ): Tej[T] =
    ???
    // Tej(j.powScalarToJet(a))

  /** pow -- base (this) is a differentiable function, exponent is a constant.
    * {{{
    * pow(a + du, p) ~= pow(a, p) + p * pow(a, p-1) du
    * }}}
    */
  def pow(p: T)(implicit
      f: Field[T],
      o: Order[T],
      s: Signed[T],
      t: Trig[T],
      v: VectorSpace[Array[T], T]
  ): Tej[T] =
    ???
    // Tej(j.pow(p))

  // As above, integer exponent.
  def pow(p: Int)(implicit f: Field[T], v: VectorSpace[Array[T], T]): Tej[T] =
    ???
    // val tmp = p * f.pow(real, p - 1)
    // Tej(j.pow(p))

  /** pow -- both base (this) and exponent are differentiable functions.
    * {{{
    * (a + du)^(b + dv) ~= a^b + b * a^(b-1) du + a^b log(a) dv
    * }}}
    */
  def pow(
      b: Tej[T]
  )(implicit
      c: ClassTag[T],
      f: Field[T],
      m: CModule[Array[T], T],
      o: Order[T],
      s: Signed[T],
      t: Trig[T]
  ): Tej[T] =
    ???
    // Tej(j.pow(b.j))

  /** log(a + du) ~= log(a) + du / a
    */
  def log(implicit
      f: Field[T],
      t: Trig[T],
      n: NRoot[T],
      v: VectorSpace[Array[T], T],
      d: TejDim[T]
  ): Tej[T] =
    d.unary(this, TejOpUrnary(UrnaryOps.Log, Tej[T](tejNum = t.log(tejNum)), this.nodeId))

  /** sqrt(a + du) ~= sqrt(a) + du / (2 sqrt(a))
    */
  def sqrt(implicit
      f: Field[T],
      n: NRoot[T],
      t: Trig[T],
      v: VectorSpace[Array[T], T],
      d: TejDim[T]
  ): Tej[T] =
    d.unary(this, TejOpUrnary(UrnaryOps.Sqrt, Tej[T](tejNum = n.sqrt(tejNum)), this.nodeId))

  /** acos(a + du) ~= acos(a) - 1 / sqrt(1 - a**2) du
    */
  def acos(implicit
      f: Field[T],
      n: NRoot[T],
      t: Trig[T],
      v: VectorSpace[Array[T], T]
  ): Tej[T] =
    ???
    // Tej(j.acos)

  /** asin(a + du) ~= asin(a) - 1 / sqrt(1 - a**2) du
    */
  def asin(implicit
      f: Field[T],
      n: NRoot[T],
      t: Trig[T],
      v: VectorSpace[Array[T], T]
  ): Tej[T] =
    ???
    // Tej(j.asin)

  /** atan(a + du) ~= atan(a) + 1 / (1 + a**2) du
    */
  def atan(implicit
      f: Field[T],
      t: Trig[T],
      v: VectorSpace[Array[T], T]
  ): Tej[T] =
    ???
    // val tmp = f.one / (f.one + real * real)
    // Tej(j.atan)
  end atan

  /** Defined with "this" as the y coordinate:
    * {{{
    * this.atan2(a) == atan2(this, a) == atan(this / a) atan2(b + dv, a + du) ~= atan2(b, a) + (- b du + a dv) / (a^2 + b^2)
    * }}}
    */
  def atan2(
      a: Tej[T]
  )(implicit f: Field[T], t: Trig[T], v: VectorSpace[Array[T], T]): Tej[T] =
    ???
    // Tej(j.atan2(a.j))

  /** exp(a + du) ~= exp(a) + exp(a) du
    */
  def exp(implicit
      t: Trig[T],
      v: VectorSpace[Array[T], T],
      d: TejDim[T],
      n: NRoot[T],
      r: Field[T]
  ): Tej[T] =
    d.unary(this, TejOpUrnary(UrnaryOps.Exp, Tej[T](tejNum = t.exp(tejNum)), this.nodeId))

  /** sin(a + du) ~= sin(a) + cos(a) du
    */
  def sin(implicit
      t: Trig[T],
      v: VectorSpace[Array[T], T],
      d: TejDim[T],
      n: NRoot[T],
      r: Field[T]
  ): Tej[T] =
    // println("sin")
    d.unary(this, TejOpUrnary(UrnaryOps.Sin, Tej(tejNum = t.sin(tejNum)), this.nodeId))

  /** sinh(a + du) ~= sinh(a) + cosh(a) du
    */
  def sinh(implicit
      t: Trig[T],
      v: VectorSpace[Array[T], T],
      d: TejDim[T],
      n: NRoot[T],
      r: Field[T]
  ): Tej[T] =
    d.unary(this, TejOpUrnary(UrnaryOps.Sinh, Tej(tejNum = t.sinh(tejNum)), this.nodeId))

  /** cos(a + du) ~= cos(a) - sin(a) du
    */
  def cos(implicit
      f: Field[T],
      t: Trig[T],
      n: NRoot[T],
      v: VectorSpace[Array[T], T],
      d: TejDim[T]
  ): Tej[T] =
    d.unary(this, TejOpUrnary(UrnaryOps.Cos, Tej(tejNum = t.cos(tejNum)), this.nodeId))

  /** cosh(a + du) ~= cosh(a) + sinh(a) du
    */
  def cosh(implicit
      t: Trig[T],
      f: Field[T],
      n: NRoot[T],
      v: VectorSpace[Array[T], T],
      d: TejDim[T]
  ): Tej[T] =
    d.unary(this, TejOpUrnary(UrnaryOps.Cosh, Tej(tejNum = t.cosh(tejNum)), this.nodeId))

  /** tan(a + du) ~= tan(a) + (1 + tan(a)**2) du
    */
  def tan(implicit
      f: Field[T],
      t: Trig[T],
      n: NRoot[T],
      v: VectorSpace[Array[T], T],
      d: TejDim[T]
  ): Tej[T] =
    d.unary(this, TejOpUrnary(UrnaryOps.Tan, Tej(tejNum = t.tan(tejNum)), this.nodeId))

  /** tanh(a + du) ~= tanh(a) + (1 - tanh(a)**2) du
    */
  def tanh(implicit
      f: Field[T],
      t: Trig[T],
      n: NRoot[T],
      v: VectorSpace[Array[T], T],
      d: TejDim[T]
  ): Tej[T] =
    d.unary(this, TejOpUrnary(UrnaryOps.Tanh, Tej(tejNum = t.tanh(tejNum)), this.nodeId))

  // Stuff needed by ScalaNumber
  override def floatValue: Float = doubleValue.toFloat
  override def doubleValue: Double = anyToDouble(tejNum)
  override def byteValue: Byte = longValue.toByte
  override def shortValue: Short = longValue.toShort
  override def intValue: Int = longValue.toInt
  override def longValue: Long = anyToLong(tejNum)
  override def underlying: Object = this
  override def isWhole: Boolean = anyIsWhole(tejNum)
  override def isValidInt: Boolean = anyIsValidInt(tejNum)

  // Object stuff
  override def hashCode: Int =
    13 * tejNum.##

  override def equals(that: Any): Boolean = that match
    case that: Tej[?] => this === that
    // case that         => isReal && real == that

  def ===(that: Tej[?]): Boolean =
    this.tejNum == that.tejNum

  def =!=(that: Tej[?]): Boolean =
    !(this === that)

  // override def toString: String =
  //   "(%s + [%s]h)".format(real.toString, infinitesimal.mkString(", "))
end Tej

trait TejInstances:
  implicit def TejAlgebra[@sp(Float, Double) T](implicit
      c: ClassTag[T],
      d: TejDim[T],
      f: Field[T],
      n: NRoot[T],
      o: Order[T],
      s: Signed[T],
      t: Trig[T]
  ): TejAlgebra[T] =
    import spire.std.array.ArrayVectorSpace
    new TejAlgebra[T]
  end TejAlgebra

  implicit def TejEq[T: Eq]: Eq[Tej[T]] = new TejEq[T]
end TejInstances

trait TejIsRing[@sp(Float, Double) T] extends Ring[Tej[T]]:
  implicit def c: ClassTag[T]
  implicit def d: TejDim[T]
  implicit def eq: Eq[T]
  implicit def f: Field[T]
  implicit def s: Signed[T]
  implicit def n: NRoot[T]
  implicit def t: Trig[T]
  implicit def v: VectorSpace[Array[T], T]

  override def minus(a: Tej[T], b: Tej[T]): Tej[T] = a - b
  def negate(a: Tej[T]): Tej[T] = -a
  def one: Tej[T] = Tej.one[T](c, d, f)
  def plus(a: Tej[T], b: Tej[T]): Tej[T] = a + b
  override def pow(a: Tej[T], b: Int): Tej[T] = a.pow(b)
  override def times(a: Tej[T], b: Tej[T]): Tej[T] = a * b
  def zero: Tej[T] = Tej.zero(c, d, f)

  override def fromInt(n: Int): Tej[T] = Tej.fromInt[T](n)
end TejIsRing

/* TODO: Tej[T] is probably not a genuine GCD ring */
trait TejIsGCDRing[@sp(Float, Double) T] extends TejIsRing[T] with GCDRing[Tej[T]] {
  /* TODO: What exactly is this GCD trying to achieve? Tests? */
  /*def gcd(a: Tej[T], b: Tej[T]): Tej[T] = {
    @tailrec def _gcd(a: Tej[T], b: Tej[T]): Tej[T] =
      if (b.isZero) a else _gcd(b, a - (a / b).round * b)
    _gcd(a, b)
  }*/
}

/* TODO: Tej[T] is probably not a genuine Euclidean ring */
trait TejIsEuclideanRing[@sp(Float, Double) T] extends TejIsGCDRing[T] with EuclideanRing[Tej[T]] {
  /*def euclideanFunction(a: Tej[T]): BigInt = sys.error("Clarify Tej first, see #598")
  /* TODO: what are exactly the laws of Tej with respect to EuclideanRing ? */
  def quot(a: Tej[T], b: Tej[T]): Tej[T] = a /~ b
  def mod(a: Tej[T], b: Tej[T]): Tej[T] = a % b
  override def quotmod(a: Tej[T], b: Tej[T]): (Tej[T], Tej[T]) = a /% b*/
}

/* TODO: Tej[T] is probably not a genuine Field */
trait TejIsField[@sp(Float, Double) T] extends TejIsEuclideanRing[T] with Field[Tej[T]]:
  /* TODO: what are exactly the laws of Tej with respect to EuclideanRing ? */
  // duplicating methods because super[..].call does not work on 2.10 and 2.11
  override def fromDouble(n: Double): Tej[T] = Tej(f.fromDouble(n))
  def div(a: Tej[T], b: Tej[T]): Tej[T] = a / b
end TejIsField

trait TejIsTrig[@sp(Float, Double) T] extends Trig[Tej[T]]:
  implicit def c: ClassTag[T]
  implicit def d: TejDim[T]
  implicit def f: Field[T]
  implicit def n: NRoot[T]
  implicit def s: Signed[T]
  implicit def t: Trig[T]
  implicit def v: VectorSpace[Array[T], T]

  def e: Tej[T] = Tej[T](t.e)
  def pi: Tej[T] = Tej[T](t.pi)

  def exp(a: Tej[T]): Tej[T] = a.exp
  def expm1(a: Tej[T]): Tej[T] = a.exp - f.one
  def log(a: Tej[T]): Tej[T] = a.log
  def log1p(a: Tej[T]): Tej[T] = (a + f.one).log

  def sin(a: Tej[T]): Tej[T] = a.sin
  def cos(a: Tej[T]): Tej[T] = a.cos
  def tan(a: Tej[T]): Tej[T] = a.tan

  def asin(a: Tej[T]): Tej[T] = a.asin
  def acos(a: Tej[T]): Tej[T] = a.acos
  def atan(a: Tej[T]): Tej[T] = a.atan
  def atan2(y: Tej[T], x: Tej[T]): Tej[T] = y.atan2(x)

  def sinh(x: Tej[T]): Tej[T] = x.sinh
  def cosh(x: Tej[T]): Tej[T] = x.cosh
  def tanh(x: Tej[T]): Tej[T] = x.tanh

  def toRadians(a: Tej[T]): Tej[T] = a
  def toDegrees(a: Tej[T]): Tej[T] = a
end TejIsTrig

trait TejIsNRoot[T] extends NRoot[Tej[T]]:
  implicit def f: Field[T]
  implicit def n: NRoot[T]
  implicit def o: Order[T]
  implicit def t: Trig[T]
  implicit def s: Signed[T]
  implicit def c: ClassTag[T]
  implicit def v: VectorSpace[Array[T], T]
  implicit def d: TejDim[T]

  def nroot(a: Tej[T], k: Int): Tej[T] = a.nroot(k)
  override def sqrt(a: Tej[T]): Tej[T] = a.sqrt
  def fpow(a: Tej[T], b: Tej[T]): Tej[T] = a.pow(b)
  def fpow(a: T, b: Tej[T]): Tej[T] = b.powScalarToTej(a)
end TejIsNRoot

@SerialVersionUID(0L)
class TejEq[T: Eq] extends Eq[Tej[T]] with Serializable:
  def eqv(x: Tej[T], y: Tej[T]): Boolean = x.eqv(y)
  override def neqv(x: Tej[T], y: Tej[T]): Boolean = x.neqv(y)
end TejEq

/* TODO
trait TejIsSigned[T] extends Signed[Tej[T]] {
  implicit def f: Field[T]
  implicit def r: IsReal[T]
  implicit def t: Trig[T]
  implicit def v: VectorSpace[Array[T],T]

  def signum(a: Tej[T]): Int = a.signum
  def abs(a: Tej[T]): Tej[T] = a.abs
  def compare(x: Tej[T], y: Tej[T]): Int = ???
}
 */

@SerialVersionUID(0L)
class TejAlgebra[@sp(Float, Double) T](implicit
    val c: ClassTag[T],
    val d: TejDim[T],
    val eq: Eq[T],
    val f: Field[T],
    val n: NRoot[T],
    val o: Order[T],
    val t: Trig[T],
    val s: Signed[T],
    val v: VectorSpace[Array[T], T]
) extends TejIsField[T]
    with TejIsTrig[T]
    with TejIsNRoot[T]
//  with TejIsSigned[T]
    with VectorSpace[Tej[T], T]
    with FieldAssociativeAlgebra[Tej[T], T]
    with Serializable:
  inline def scalar: Field[T] = f
  inline def nroot: NRoot[T] = n
  def timesl(a: T, w: Tej[T]): Tej[T] = Tej(a) * w
  def dot(x: Tej[T], y: Tej[T]): T = ???
  // x.tejNum.infinitesimal
  //   .zip(y.j.infinitesimal)
  //   .foldLeft(scalar.times(x.j.real, y.j.real)) { (xx, yy) =>
  //     scalar.plus(xx, scalar.times(yy._1, yy._2))
  //   }
end TejAlgebra
