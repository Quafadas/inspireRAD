# Automatic Differentation

### Scala cli
```scala
//> using dep io.github.quafadas::spireAD::{{projectVersion}}
```

### Mill
```scala sc:nocompile
ivy"io.github.quafadas::spireAD::{{projectVersion}}"
```

## goals

implement a reverse automatic differentation algorithm.

Correctness (in terms of operations)

Adopt Spires typeclass driven design

Reasonable ease of use at the call site

Reasonable hardware accelerated (CPU) performance

## non goals
(at this stage)
performance

vectorisation

