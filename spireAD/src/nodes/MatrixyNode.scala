package io.github.quafadas.spireAD

import vecxt.matrix.Matrix
import java.util.UUID
import cats.syntax.show.*
import cats.Show

case class MatrixyNode[T](
    op: MatrixyBinaryOps,
    value1: Matrix[T],
    thisId: UUID,
    left: UUID,
    right: UUID
)(using
    vf: VectorisedField[Matrix, T],
    vt: VectorisedTrig[Matrix, T],
    maty: Matrixy[Matrix, T],
    sh: Show[Matrix[T]]
) extends VNode[Matrix, T](value1, thisId):

  override def graphShow: String =
    s"MatNode (id: ${thisId.toString().takeRight(4)}, op: $op, value: ${value.show})"

  override def backward[N <: VDimChangeNode[?, ?, T]](using td: TejVGraph[T]): Unit =
    op match
      case MatrixyBinaryOps.MatMul =>
        val leftN = td.dag.getNode(left).asInstanceOf[MatrixyNode[T]]
        val rightN = td.dag.getNode(right).asInstanceOf[MatrixyNode[T]]
        leftN.grad = vf.+(leftN.grad)(maty.matmul(this.grad)(rightN.value.transpose))
        rightN.grad = vf.+(rightN.grad)(maty.matmul(rightN.value.transpose)(this.grad))

end MatrixyNode
