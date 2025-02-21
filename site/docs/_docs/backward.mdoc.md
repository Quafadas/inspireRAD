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

def softmax[T: Trig: ClassTag](x: Array[T])(using
  f: Field[T]
) = {
  val exps = x.map(exp)
  val sumExps = exps.foldLeft(f.zero)(_ + _)
  exps.map(t => t  / sumExps)
}

def sumSin[T: Trig: ClassTag](x: Array[T])(using
  f: Field[T]
) = {
  x.map(sin).foldLeft(f.zero)(_ + _)
}


given jd: TejDim[Double] = TejDim()
val dim = 5
val range = (1 to dim).toArray.map(_.toDouble)

sumSin(softmax[Tej[Double]](range))
val traced : Tej[Double] = sumSin(softmax[Tej[Double]](range.tejArr))

/// And now we have... exaclty the same number! So yey?

val t  = jd.dag.toGraphviz

// traced.backward()

```
