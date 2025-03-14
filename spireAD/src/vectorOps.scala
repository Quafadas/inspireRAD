package io.github.quafadas.spireAD

enum UrnaryOps:
  case Sin, Cos, Tan, Exp, Log, Sinh, Cosh, Tanh, Neg, Sqrt, Abs
end UrnaryOps

enum BinaryOps:
  case Add, Sub, Mul, Div
end BinaryOps

enum ReductionOps:
  case Sum, Product, Mean
end ReductionOps

enum MatrixyOps:
  case Index, MatMul, MapRows, MapRowsToScalar
end MatrixyOps
