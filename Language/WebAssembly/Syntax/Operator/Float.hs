module Language.WebAssembly.Syntax.Operator.Float where

import GHC.TypeLits (Nat)

import Language.WebAssembly.Syntax.Operator.Kind

data family FloatOp :: Nat -> * -> *

data instance FloatOp bits Unary
   = Neg | Abs | Ceil | Floor | Trunc | Nearest | Sqrt
   deriving (Show)

data instance FloatOp bits Binary
   = Add | Sub | Mul | Div | Min | Max | CopySign
   deriving (Show)

data instance FloatOp bits Compare
   = Eq | Ne | Lt | Gt | Le | Ge
   deriving (Show)

data instance FloatOp bits Convert
   = ConvertSI32 | ConvertUI32 | ConvertSI64 | ConvertUI64 | PromoteF32 | DemoteF64 | ReinterpretInt
   deriving (Show)

type F32Op = FloatOp 32
type F64Op = FloatOp 64
