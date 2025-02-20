---
title: Forward Mode
---

We've already seen that Spire provides the abillity to get the derivate of a function at a point through it's `Jet` class. Through function composition, we can find the derivate of a pretty much arbitrary function. 

```scala

import spire._
import spire.math._
import spire.implicits.*
import _root_.algebra.ring.Field
import spire.algebra.Trig

import spire.math.Jet.*
import io.github.quafadas.spireAD.*


def softmax[T](x: Array[T])(using
  t: Trig[T],    
  f: Field[T],  
  ct: ClassTag[T]  
) = {    
  val exps = x.map(exp)
  val sumExps = exps.foldLeft(f.zero)(_ + _)
  exps.map(t => t  / sumExps)
}

val upper = 4
given jd: JetDim = JetDim(upper)
val range = (1 to upper).toArray

softmax[Double](range)
softmax[Jet[Double]](range.jetArr)


```