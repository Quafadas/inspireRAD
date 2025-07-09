package io.github.quafadas.spireAD

import vecxt.matrix.Matrix
import java.util.UUID
import cats.syntax.show.*
import cats.Show
import narr.*
import algebra.ring.Field

import scala.reflect.ClassTag
import vecxt.all.mapRowsInPlace
import vecxt.all.mapRows
import vecxt.all.row

case class MatrixyNode[T](
    op: MatrixyBinaryOps,
    value1: Matrix[T],
    thisId: UUID,
    left: UUID,
    right: UUID
)(using
    vf: VectorisedField[Matrix, T],
    vfa: VectorisedField[Array, T],
    vt: VectorisedTrig[Matrix, T],
    f: Field[T],
    maty: Matrixy[Matrix, T],
    sh: Show[Matrix[T]],
    ct: ClassTag[T]
) extends VNode[Matrix, T](value1, thisId):
  val vfa1 = vfa
  override def graphShow: String =
    s"MatNode (id: ${thisId.toString().takeRight(4)}, op: $op, value: ${value.show})"

  // def setGradOne(using ct: ClassTag[T]): Unit =
  //   grad = vf.allOnes(grad)

  override def backward[N <: VDimChangeNode[?, ?, T]](using td: TejVGraph[T]): Unit =
    op match
      case MatrixyBinaryOps.MatMul =>
        val leftN = td.dag.getNode(left).asInstanceOf[VDimChangeNode[Matrix, Matrix, T]]
        val rightN = td.dag.getNode(right).asInstanceOf[VDimChangeNode[Matrix, Matrix, T]]
        leftN.grad = vf.+(leftN.grad)(maty.matmul(this.grad)(rightN.value.transpose))
        rightN.grad = vf.+(rightN.grad)(maty.matmul(leftN.value.transpose)(this.grad))
      case MatrixyBinaryOps.AddToRows =>
        val leftN = td.dag.getNode(left).asInstanceOf[MatrixyNode[T]]
        val rightN = td.dag.getNode(right).asInstanceOf[VNode[Array, T]]
        val newGrad = vf.allOnes(value1)    
        leftN.grad += newGrad

        val rows = value1.rows
        val ones = vfa.allOnes(value1.row(0))
        val newGradR = vfa.*(ones)( f.fromInt(rows))
        rightN.grad += newGradR
        


end MatrixyNode

// case class ExplodeNode[T](
//     value1: Matrix[T],
//     thisId: UUID,
//     rowTransforms: Seq[(UUID, UUID)]
// )(using
//     vf: VectorisedField[Matrix, T],

//     vt: VectorisedTrig[Matrix, T],
//     // maty: Matrixy[Matrix, T],
//     sh: Show[Matrix[T]]
// ) extends VNode[Matrix, T](value1, thisId):

//   override def graphShow: String =
//     s"ExplodeNode (id: ${thisId.toString().takeRight(4)})"

//   override def backward[N <: VDimChangeNode[?, ?, T]](using td: TejVGraph[T]): Unit =
//     val rows = for n <- rowTransforms yield
//       td.dag.getNode(n._1).value

//     println(rows)

// case class AggregateNode[T](
//     value1: Matrix[T],
//     thisId: UUID,
//     rowTransforms: Seq[(UUID, UUID)]
// )(using
//     vf: VectorisedField[Matrix, T],

//     vt: VectorisedTrig[Matrix, T],
//     // maty: Matrixy[Matrix, T],
//     sh: Show[Matrix[T]],
//     ct: ClassTag[T]
// ) extends VNode[Matrix, T](value1, thisId):

//   override def graphShow: String =
//     s"AggregateNode (id: ${thisId.toString().takeRight(4)})"

//   override def backward[N <: VDimChangeNode[?, ?, T]](using td: TejVGraph[T]): Unit =
//     println("backward pass for AggregateNode")
//     for (n, i) <- rowTransforms.zipWithIndex yield
//       val newArr = grad.asInstanceOf[Matrix[T]].row(i)
//       val newNode = td.dag.getNode(n._2)

//       // println(newArr.mkString("[", ", ", "]"))
//       // OMG ths is terrible....
//       for (j <- 0 until newArr.length) do
//         newNode.grad.asInstanceOf[Array[T]](j) = newArr(j)

//       println(s"newNode: ${newNode.graphShow}")
//       // println(newNode.grad.mkString("[", ", ", "]"))
//       // println(s"AggregateNode backward: ${thisId.to

//     // println(rows)

case class RowReductionNode[T](
    op: ReductionOps,
    value1: Array[T],
    thisId: UUID,
    depId: UUID,
    someGtoSetGrad: Array[T]
)(using
    gfa: VectorisedField[Array, T],
    gft: VectorisedTrig[Array, T],
    svf: Field[T],
    shF: Show[Array[T]],
    shG: Show[Matrix[T]],
    ct: ClassTag[T]
) extends VDimChangeNode[Array, Array, T](value1, thisId):

  var grad: Array[T] = gfa.zero(someGtoSetGrad)

  def setGradOne(using ct: ClassTag[T]): Unit =
    grad = gfa.allOnes(grad)

  def setGradZero(using ct: ClassTag[T]): Unit =
    grad = gfa.zero(grad)

  override def graphShow: String =
    s"RowReductionNode (id: ${thisId.toString().takeRight(4)}, op: $op, value: ${value.show})"

  override def backward[N <: VDimChangeNode[?, ?, T]](using td: TejVGraph[T]): Unit =
    val n = td.dag.getNode(depId).asInstanceOf[VNode[Matrix, T]]
    var i = 0
    // println(s"RowReductionNode backward: ${thisId.toString().takeRight(4)}")
    // println("RowGrads: " + grad.asInstanceOf[Array[T]].mkString("[", ", ", "]"))
    val incomingGrad = grad.asInstanceOf[Array[T]]
    op match
      case ReductionOps.Sum =>
        n.grad
          .asInstanceOf[Matrix[T]]
          .mapRowsInPlace(row =>
            val newRow = gfa.+(row)(incomingGrad(i))
            i += 1
            newRow
          )

      case ReductionOps.Product =>
        n.grad = n.value.mapRows(row =>
          val newRow = row.productExceptSelf() * incomingGrad(i)
          i += 1
          newRow
        )
      case ReductionOps.Mean =>
        n.grad
          .asInstanceOf[Matrix[T]]
          .mapRowsInPlace(row =>
            val broadCast = svf.div(incomingGrad(i), svf.fromInt(row.length))
            val newRow = gfa.+(row)(broadCast)
            i += 1
            newRow
          )
    end match
  end backward
end RowReductionNode
