package io.github.quafadas.spireAD

import scala.math.*

inline def anyIsZero(n: Any): Boolean =
  n match
    case x if x == 0                => true
    case c: ScalaNumericConversions => c.isValidInt && c.toInt == 0
    case _                          => false

inline def anyToDouble(n: Any): Double =
  n match
    case n: Byte                    => n.toDouble
    case n: Short                   => n.toDouble
    case n: Char                    => n.toDouble
    case n: Int                     => n.toDouble
    case n: Long                    => n.toDouble
    case n: Float                   => n.toDouble
    case n: Double                  => n
    case c: ScalaNumericConversions => c.toDouble
    case _ =>
      throw new UnsupportedOperationException(s"$n is not a ScalaNumber")

inline def anyToLong(n: Any): Long =
  n match
    case n: Byte                    => n.toLong
    case n: Short                   => n.toLong
    case n: Char                    => n.toLong
    case n: Int                     => n.toLong
    case n: Long                    => n
    case n: Float                   => n.toLong
    case n: Double                  => n.toLong
    case c: ScalaNumericConversions => c.toLong
    case _ =>
      throw new UnsupportedOperationException(s"$n is not a ScalaNumber")

inline def anyIsWhole(n: Any): Boolean =
  n match
    case _: Byte                    => true
    case _: Short                   => true
    case _: Char                    => true
    case _: Int                     => true
    case _: Long                    => true
    case n: Float                   => n.isWhole
    case n: Double                  => n.isWhole
    case c: ScalaNumericConversions => c.isWhole
    case _ =>
      throw new UnsupportedOperationException(s"$n is not a ScalaNumber")

inline def anyIsValidInt(n: Any): Boolean =
  n match
    case _: Byte                    => true
    case _: Short                   => true
    case _: Char                    => true
    case _: Int                     => true
    case n: Long                    => n.isValidInt
    case n: Float                   => n.isValidInt
    case n: Double                  => n.isValidInt
    case c: ScalaNumericConversions => c.isValidInt
    case _ =>
      throw new UnsupportedOperationException(s"$n is not a ScalaNumber")
