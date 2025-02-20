package io.github.quafadas.spireAD

import java.util.UUID
import java.util as ju
import algebra.ring.Field
import spire.algebra.Trig
import spire.implicits.*
import spire.algebra.Trig.*
import spire.math.cos
import spire.math.sin
import spire.math.cosh
import spire.math.sinh
import scala.reflect.ClassTag
import spire.algebra.NRoot

trait AdNode[T: Field]:
  def id: UUID
  val realValue: T
  val infinitesimal: Array[T]
  var grad: T = summon[Field[T]].zero // Gradient accumulator
  def backward(using td: TejDim[T]): Unit
end AdNode
case class DebugNode[T: Field: ClassTag](msg: String) extends AdNode[T]:

  override val realValue: T = summon[Field[T]].zero

  override def backward(using td: TejDim[T]): Unit = {}

  val n = UUID.randomUUID()
  override inline def id: UUID = n
  val infinitesimal: Array[T] = Array.empty
end DebugNode
case class TejNode[T: Field](tej: Tej[T]) extends AdNode[T]:
  override def id: UUID = tej.nodeId

  override val realValue: T = tej.j.real
  override val infinitesimal: Array[T] = tej.j.infinitesimal

  override def toString(): String =
    s"const \n v:$realValue g: $grad \n (_id: ${id.toString().takeRight(4)})"

  override def backward(using td: TejDim[T]): Unit = ()
end TejNode

enum UrnaryOps:
  case Sin, Cos, Tan, Exp, Log, Sinh, Cosh, Tanh, Neg, Sqrt, Abs
end UrnaryOps

case class TejOpUrnary[T: Field: Trig: NRoot](
    op: UrnaryOps,
    value: Tej[T],
    dep: UUID
) extends AdNode[T]:
  override def id: UUID = value.nodeId

  override val realValue: T = value.j.real
  override val infinitesimal: Array[T] = value.j.infinitesimal

  override def toString(): String =
    s"$op \n v:$value g: $grad \n (_id: ${value.nodeId.toString().takeRight(4)})"

  override def backward(using td: TejDim[T]): Unit =
    val n = td.dag.getNode(dep)
    op match
      case UrnaryOps.Sin => n.grad = n.grad + this.grad * cos(n.realValue)
      case UrnaryOps.Cos => n.grad = n.grad - this.grad * sin(n.realValue)
      case UrnaryOps.Tan => n.grad = n.grad + this.grad / (cos(n.realValue) * cos(n.realValue))
      case UrnaryOps.Exp => n.grad = n.grad + this.grad * n.realValue.exp
      case UrnaryOps.Log =>
        n.grad = n.grad + this.grad / n.realValue
      case UrnaryOps.Sinh => n.grad = n.grad + this.grad * cosh(n.realValue)
      case UrnaryOps.Cosh => n.grad = n.grad + this.grad * sinh(n.realValue)
      case UrnaryOps.Tanh => n.grad = n.grad + this.grad / (cosh(n.realValue) * cosh(n.realValue))
      case UrnaryOps.Neg  => n.grad = n.grad - this.grad
      case UrnaryOps.Sqrt => n.grad = n.grad + this.grad / (2 * sqrt(n.realValue))
      case _              => ???
    end match

    // n.grad = td.dag.getNode(dep).grad + this.grad
  end backward
end TejOpUrnary

enum BinaryOps:
  case Add, Sub, Mul, Div
end BinaryOps

case class TejOpBinary[T: Field](
    op: BinaryOps,
    value: Tej[T],
    left: UUID,
    right: UUID
) extends AdNode[T]:
  override def id: UUID = value.nodeId

  override val realValue: T = value.j.real
  override val infinitesimal: Array[T] = value.j.infinitesimal

  override def toString(): String =
    s"$op \n v:$value g: $grad \n (_id: ${value.nodeId.toString().takeRight(4)})"

  override def backward(using td: TejDim[T]): Unit =
    val leftN = td.dag.getNode(left)
    val rightN = td.dag.getNode(right)
    op match
      case BinaryOps.Add =>
        leftN.grad = leftN.grad + this.grad
        rightN.grad = rightN.grad + this.grad
      case BinaryOps.Sub =>
        leftN.grad += this.grad
        rightN.grad -= this.grad

      case BinaryOps.Mul =>
        leftN.grad += this.grad * rightN.realValue
        rightN.grad += this.grad * leftN.realValue

      case BinaryOps.Div =>
        leftN.grad += this.grad / rightN.realValue
        rightN.grad -= this.grad * leftN.realValue / (rightN.realValue * rightN.realValue)
    end match
  end backward
end TejOpBinary

def graphShow(adNode: AdNode[?]): String = adNode match
  case DebugNode(msg) => msg
  case TejNode(tej)   => tej.toString
  case TejOpUrnary(op, value, dep) =>
    s"$op \n $value \n (_id: ${(value.nodeId.toString().takeRight(4))})"
  case TejOpBinary(op, value, left, right) =>
    s"$op \n $value \n (_id: ${(value.nodeId.toString().takeRight(4))})"
end graphShow
