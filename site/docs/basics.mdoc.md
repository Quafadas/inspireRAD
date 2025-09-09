---
title: Basics
---


## Basics of Differentiation

To start from the very start. Let's square a number.
```scala mdoc
import scala.math.*

def sq(x: Double) = x * x

List(1.0, 2.0, 3.0).map(sq)

```
Something that you'll notice, is that the result of the square of a number increases faster than the increase in the inputs. The rate of increase of a function is called it's derivative. Math say;

<math xmlns="http://www.w3.org/1998/Math/MathML" display="block">
  <mrow>
    <mi>f</mi>
    <mo>(</mo>
    <mi>x</mi>
    <mo>)</mo>
    <mo>=</mo>
    <msup>
      <mi>x</mi>
      <mn>2</mn>
    </msup>
  </mrow>
</math>

<math xmlns="http://www.w3.org/1998/Math/MathML" display="block">
  <mrow>
    <msup>
      <mi>f</mi>
      <mo>'</mo>
    </msup>
    <mo>(</mo>
    <mi>x</mi>
    <mo>)</mo>
    <mo>=</mo>
    <mn>2</mn>
    <mi>x</mi>
  </mrow>
</math>

Which is one mathematical notation of a derivative. Such derivaties can (sometimes!) be derived symbolically (see chat GPT or a math textbook), but also numerically at a point.

## “Duel” ands “Jets"

In case you didn't read Spire's scaladoc yet, you should. I've copied and pasted this bit.

While a complete treatment of the mechanics of automatic differentiation is beyond the scope of this header (see http://en.wikipedia.org/wiki/Automatic_differentiation for details), the basic idea is to extend normal arithmetic with an extra element "h" such that <math xmlns="http://www.w3.org/1998/Math/MathML" display="inline">
  <msup>
    <mi>h</mi>
    <mn>2</mn>
  </msup>
  <mo>=</mo>
  <mn>0</mn>
</math>

h itself is non zero - an infinitesimal.

Dual numbers are extensions of the real numbers analogous to complex numbers: whereas complex numbers augment the reals by introducing an imaginary unit i such that <math xmlns="http://www.w3.org/1998/Math/MathML" display="inline">
  <msup>
    <mi>i</mi>
    <mn>2</mn>
  </msup>
  <mo>=</mo>
  <mo>-</mo>
  <mn>1</mn>
</math>

Dual numbers introduce an "infinitesimal" unit h such that <math xmlns="http://www.w3.org/1998/Math/MathML" display="inline">
  <msup>
    <mi>h</mi>
    <mn>2</mn>
  </msup>
  <mo>=</mo>
  <mn>0</mn>
</math>. Analogously to a complex number <math xmlns="http://www.w3.org/1998/Math/MathML" display="inline">
  <mi>c</mi>
  <mo>=</mo>
  <mi>x</mi>
  <mo>+</mo>
  <mi>y</mi>
  <mo>&#x22C5;</mo>
  <mi>i</mi>
</math>, a dual number <math xmlns="http://www.w3.org/1998/Math/MathML" display="inline">
  <mi>d</mi>
  <mo>=</mo>
  <mi>x</mi>
  <mo>+</mo>
  <mi>y</mi>
  <mo>&#x22C5;</mo>
  <mi>h</mi>
</math> has two components: the "real" component x, and an "infinitesimal" component y. Surprisingly, this leads to a convenient method for computing exact derivatives without needing to manipulate complicated symbolic expressions.

For example, consider the function
<math xmlns="http://www.w3.org/1998/Math/MathML" display="block">
  <mrow>
    <mi>f</mi>
    <mo>(</mo>
    <mi>x</mi>
    <mo>)</mo>
    <mo>=</mo>
    <mi>x</mi>
    <mo>&#x22C5;</mo>  <!-- Multiplication dot -->
    <mi>x</mi>
  </mrow>
</math>

 evaluated at 10. Using normal arithmetic,

```
f(10 + h) = (10 + h) * (10 + h)
          = 100 + 2 * 10 * h + h * h
          = 100 + 20 * h       +---
                    +-----       |
                    |            +--- This is zero
                    |
                    +----------------- This is df/dx
```
Spire offers us the ability to compute derivatives using Dual numbers through it's `Jet` implementation.

```scala mdoc
import spire._
import spire.math._
import spire.implicits.*
import spire.math.Jet.*

given jd: JetDim = JetDim(1)
val y = Jet(10.0) + Jet.h[Double](0)
y * y
```
Where we tracked the derivative of the first dimension.
