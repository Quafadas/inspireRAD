---
title: Backward Mode
---

So we got forward mode, essentially free. Sweet.

However, if you look into the code for `Jet`, what you'll see, is that it's tracking the gradient of every partial derivative, through every calculation. If you were to have a large number of input dimensions (e.g. training a neural net) this calculation gets expensive. I believe it to be O(n).

The motivation for reverse mode differentiation, is that it is O(1).

The implementation is fascinating from both a maths and implementation perspective. The reverse mode AD algorithm uses some cunning mathematics (not described here) and the directed (acyclic) graph of the calcualtion at that point. To use it;

```scala mdoc:height=200

import spire._
import spire.math._
import spire.implicits.*
import _root_.algebra.ring.Field
import spire.algebra.Trig

import spire.math.Jet.*
import io.github.quafadas.spireAD.*

def softmax[T: Trig: ClassTag](x: Array[T])(using f: Field[T]): Array[T] =
  val expValues = x.map(exp)
  val sumExpValues = expValues.foldLeft(f.zero)(_ + _)
  expValues.map(_ / sumExpValues)
end softmax

given jd: TejDim[Double] = TejDim()
val dim = 5
val range = (1 to dim).toArray.map(_.toDouble).tejArr

val traced : Tej[Double] = softmax(range).foldLeft(Tej(0.0))(_+_)

/// And now we have... exaclty the same number! So yey?

traced.backward(range)

```
`traced.backward(range)` does the backward pass, and returns a tuple of the gradient of the `traced` variable, with respect to each of the input dimensions (that you asked for in `range`).

It is possible to inspect the calculation graph which is carried around in side the `TejDim` given.

`jd.dag.toGraphviz` will return a string representation of the graph. If you paste the output of the calculation graph into an online graphviz editor, you'll see something like this.

![backward](backward.png)

To do it, it navigates the graph in reverse topological order, propagating its gradient to its child nodes in accordance with the chain rule.

