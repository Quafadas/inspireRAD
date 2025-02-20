---
title: Forward Mode
---

We've already seen that Spire provides the abillity to get the derivate of a function at a point through it's `Jet` class. Through function composition, we can find the derivate of a pretty much arbitrary function. 

```scala mdoc

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

val dim = 4
given jd: JetDim = JetDim(dim)
val range = (1 to dim).toArray.map(_.toDouble)

softmax[Double](range)
softmax[Jet[Double]](range.jetArr)

sumSin(softmax[Double](range))
sumSin(softmax[Jet[Double]](range.jetArr))

```

Once you're past the (somewhat formiddable ) list of Spire's typeclasses, we can use function composition to track  the derivaties of arbitrarily complex functions. Pretty neat!

This is forward mode automatic differentation. 