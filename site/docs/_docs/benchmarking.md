
Here is a benchmark of softmax for varying parameter sizes. Note this on my local computer. It appears as thought the forward pass method dominates below about 250 parameters.

```shell

Benchmark                    (len)   Mode  Cnt        Score       Error  Units
spireAD.TejJetBenchmark.jet      3  thrpt    3  1075139.427 ± 34410.674  ops/s
spireAD.TejJetBenchmark.jet    250  thrpt    3      448.383 ±    45.942  ops/s
spireAD.TejJetBenchmark.jet  10000  thrpt    3        0.310 ±     0.020  ops/s
spireAD.TejJetBenchmark.tej      3  thrpt    3    36925.357 ±   842.474  ops/s
spireAD.TejJetBenchmark.tej    250  thrpt    3      406.030 ±   178.005  ops/s
spireAD.TejJetBenchmark.tej  10000  thrpt    3        4.539 ±     4.848  ops/s

```