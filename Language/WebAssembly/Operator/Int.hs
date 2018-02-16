{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Language.WebAssembly.Operator.Int where

import GHC.TypeLits (Nat)

import Language.WebAssembly.Operator.Generic

data family IntOp :: Nat -> * -> *

data instance IntOp bits Unary
   = Clz | Ctz | Popcnt
   deriving (Show)

data instance IntOp bits Binary
   = Add | Sub | Mul | DivS | DivU | RemS | RemU | And | Or | Xor | Shl | ShrS | ShrU | Rotl | Rotr
   deriving (Show)

data instance IntOp bits Test
   = Eqz
   deriving (Show)

data instance IntOp bits Compare
   = Eq | Ne | LtS | LtU | GtS | GtU | LeS | LeU | GeS | GeU
   deriving (Show)

data instance IntOp bits Convert
   = ExtendSI32 | ExtendUI32 | WrapI64 | TruncSF32 | TruncUF32 | TruncSF64 | TruncUF64 | ReinterpretFloat
   deriving (Show)

type I32Op = IntOp 32
type I64Op = IntOp 64
