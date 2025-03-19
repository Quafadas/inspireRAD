package io.github.quafadas.spireAD

enum UrnaryOps:
  case Sin, Cos, Tan, Exp, Log, Sinh, Cosh, Tanh, Neg, Sqrt, Abs, Index
end UrnaryOps

enum BinaryOps:
  case Add, Sub, Mul, Div
end BinaryOps

enum ReductionOps:
  case Sum, Product, Mean
end ReductionOps

enum ParameterisedReductionOps[N](val parameter: TupleDim[N]):
  case Index[N](override val parameter: TupleDim[N]) extends ParameterisedReductionOps(parameter)
  case Update[N](override val parameter: TupleDim[N]) extends ParameterisedReductionOps(parameter)
end ParameterisedReductionOps

enum MatrixyBinaryOps:
  case MatMul
end MatrixyBinaryOps
