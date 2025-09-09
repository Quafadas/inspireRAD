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

implement a reverse automatic differentation algorithm in Scala on top of spire.

Completeness (in terms of operations)

Adopt Spires typeclass driven design

Accuracy

Reasonable ease of use at the call site

## non goals
(at this stage)
performance

vectorisation

