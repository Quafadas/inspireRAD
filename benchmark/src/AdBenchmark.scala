package io.github.quafadas.inspireRAD

import org.openjdk.jmh.annotations.*

import java.util.Random
import java.util.concurrent.TimeUnit
import scala.compiletime.uninitialized

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@State(Scope.Thread)
@Fork(value = 1)
@Warmup(iterations = 2)
@Measurement(iterations = 3)
abstract class AdBenchmark:

  @Setup
  def setupImplementation: Unit = ()

  private final val rand: Random = new Random(0);

  protected def randomDouble(): Double =
    return rand.nextDouble();

  protected def randomInt(): Double =
    return rand.nextInt();

  protected def randomDoubleArray(n: Int): Array[Double] =
    val res = new Array[Double](n);

    for i <- 0 until n do res(i) = rand.nextDouble();
    end for
    return res;
  end randomDoubleArray
end AdBenchmark
