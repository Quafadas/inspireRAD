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

enum ParameterisedReductionOps:
  case Index, Update
end ParameterisedReductionOps

enum MatrixyBinaryOps:
  case MatMul
end MatrixyBinaryOps
