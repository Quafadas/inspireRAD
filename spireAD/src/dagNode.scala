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
import scala.specialized as sp

trait AdNode[@sp(Float, Double) T: Field]:
  def id: UUID
  val realValue: T
  // val infinitesimal: Array[T]
  var grad: T = summon[Field[T]].zero // Gradient accumulator
  def backward(using td: TejDim[T], t: Trig[T], n: NRoot[T]): Unit
end AdNode
case class DebugNode[@sp(Float, Double) T: Field: ClassTag](msg: String) extends AdNode[T]:

  override val realValue: T = summon[Field[T]].zero

  override def backward(using td: TejDim[T], t: Trig[T], n: NRoot[T]): Unit = {}

  val n = UUID.randomUUID()
  override def id: UUID = n
end DebugNode
case class TejNode[@sp(Float, Double) T: Field](tej: Tej[T]) extends AdNode[T]:
  override def id: UUID = tej.nodeId

  override val realValue: T = tej.value
  // override val infinitesimal: Array[T] = tej.j.infinitesimal

  override def toString(): String =
    s"const \n v:$realValue g: $grad \n (_id: ${id.toString().takeRight(4)})"

  override def backward(using td: TejDim[T], t: Trig[T], n: NRoot[T]): Unit =
    ()
    // println("no op" + toString())
end TejNode

case class TejOpUrnary[@sp(Float, Double) T: Field](
    op: UrnaryOps,
    value: Tej[T],
    dep: UUID
) extends AdNode[T]:
  override def id: UUID = value.nodeId

  override val realValue: T = value.value
  // override val infinitesimal: Array[T] = value

  override def toString(): String =
    s"$op \n v:$value g: $grad \n (_id: ${value.nodeId.toString().takeRight(4)})"

  override def backward(using td: TejDim[T], t: Trig[T], n: NRoot[T]): Unit =
    val n = td.dag.getNode(dep)
    val update = op match
      case UrnaryOps.Sin  => this.grad * cos(n.realValue)
      case UrnaryOps.Cos  => -this.grad * sin(n.realValue)
      case UrnaryOps.Tan  => this.grad / (cos(n.realValue) * cos(n.realValue))
      case UrnaryOps.Exp  => this.grad * n.realValue.exp
      case UrnaryOps.Log  => this.grad / n.realValue
      case UrnaryOps.Sinh => this.grad * cosh(n.realValue)
      case UrnaryOps.Cosh => this.grad * sinh(n.realValue)
      case UrnaryOps.Tanh => this.grad / (cosh(n.realValue) * cosh(n.realValue))
      case UrnaryOps.Neg  => -this.grad
      case UrnaryOps.Sqrt => this.grad / (2 * sqrt(n.realValue))
      case _              => ???

    n.grad = n.grad + update
    // n.grad = td.dag.getNode(dep).grad + this.grad
    // println("--->backward unary" + this.toString())
    // println("New grad backward: " + n)
    // println("Updated: by " + update)
    // println("<--- end backward this node")
  end backward
end TejOpUrnary

case class TejOpBinary[@sp(Float, Double) T: Field](
    op: BinaryOps,
    value: Tej[T],
    left: UUID,
    right: UUID
) extends AdNode[T]:
  override inline def id: UUID = value.nodeId

  override val realValue: T = value.value

  override def toString(): String =
    s"$op \n v:$value g: $grad \n (_id: ${value.nodeId.toString().takeRight(4)})"

  override def backward(using td: TejDim[T], t: Trig[T], n: NRoot[T]): Unit =
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
    // println("--> backward binary" + this.toString())
    // println("Update Left: " + leftN)
    // println("Update Right: " + rightN)
    // println("<--- end backward this node")
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
