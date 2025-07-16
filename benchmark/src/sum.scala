/*
 * Copyright 2020, 2021, Ludovic Henry
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * Please contact git@ludovic.dev or visit ludovic.dev if you need additional
 * information or have any questions.
 */

package io.github.quafadas.inspireRAD

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
// import vecxt.Matrix.*
import vecxt.BoundsCheck
import scala.compiletime.uninitialized
import jdk.incubator.vector.VectorSpecies
import jdk.incubator.vector.VectorOperators
import jdk.incubator.vector.DoubleVector
import spire.*
import _root_.algebra.ring.Field
import spire.implicits.DoubleAlgebra
import spire.math.*
import spire.implicits.*
import spire.algebra.Trig
import scala.reflect.ClassTag
import spire.compat.fractional

class TejJetBenchmark extends AdBenchmark:

  @Param(Array("3", "250", "10000", "25000"))
  var len: String = uninitialized;

  var arr1: Array[Jet[Double]] = uninitialized
  var arr2: Array[Tej[Double]] = uninitialized

  var jdd: JetDim = uninitialized
  var tdd: TejDim[Double] = uninitialized

  // format: off
  @Setup(Level.Invocation)
  def setup: Unit =
    given jd: JetDim = JetDim(len.toInt)
    given td: TejDim[Double] = TejDim()

    jdd = jd
    tdd = td

    val arr = randomDoubleArray(len.toInt)

    arr1 = arr.jetArr
    arr2 = arr.tejArr
    ()

  end setup

  inline def softmax[T: Trig: ClassTag](x: Array[T])(using inline f: Field[T]): Array[T] =
    val expValues = x.map(x => exp(x) + f.one * 10 )
    val sumExpValues = expValues.foldLeft(f.zero)(_ + _)
    expValues.map(_ / sumExpValues)
  end softmax


  @Benchmark
  def jet(bh: Blackhole) =
    given jd: JetDim = jdd
    val r = softmax(arr1).foldLeft(Jet(0.0))(_ + _)
    val grads = r.infinitesimal
    bh.consume(grads);
  end jet


  @Benchmark
  def tej(bh: Blackhole) =
    given td: TejDim[Double] = tdd
    val r = softmax(arr2).foldLeft(Tej(0.0))(_ + _)
    val grads = r.backward(arr2).map(_._2)
    bh.consume(grads)
  end tej