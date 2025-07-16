package io.github.quafadas.spireAD

enum UrnaryOps:
  case Sin, Cos, Tan, Exp, Log, Sinh, Cosh, Tanh, Neg, Sqrt, Abs, Index
end UrnaryOps

enum BinaryOps:
  case Add, Sub, Mul, Div
end BinaryOps

enum BinaryScalarOps:
  case Add, Sub, Mul, Div, ClampMin
end BinaryScalarOps

enum ReductionOps:
  case Sum, Product, Mean
end ReductionOps

enum ParameterisedReductionOps[N](val parameter: TupleDim[N]):
  case Index[N](override val parameter: TupleDim[N]) extends ParameterisedReductionOps(parameter)

end ParameterisedReductionOps

enum ParameterisedUpdateOps[N](val parameter: TupleDim[N]):
  case Update[N](override val parameter: TupleDim[N]) extends ParameterisedUpdateOps(parameter)
end ParameterisedUpdateOps

enum MatrixyBinaryOps:
  case MatMul, AddToRows
end MatrixyBinaryOps

enum NormaliseRowOps:
  case NormaliseRowsL1, NormaliseRowsL2, Softmax, LogSoftmax
end NormaliseRowOps
