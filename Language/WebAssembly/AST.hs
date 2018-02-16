module Language.WebAssembly.AST where

import Data.Int   (Int32)
import Data.Maybe (fromJust, isNothing)
import Data.Word  (Word32)

import Language.WebAssembly.Operator
import Language.WebAssembly.Source
import Language.WebAssembly.Type

matchLimit :: Limit Word32 -> Limit Word32 -> Bool
matchLimit lim1 lim2 =
   l1 >= l2 && if
   | isNothing u1 -> False
   | isNothing u2 -> True
   | otherwise    -> fromJust u1 <= fromJust u2
   where Limit l1 u1 = lim1
         Limit l2 u2 = lim2

type Var = Phrase Int32
type Lit = Phrase ValueType

data Expr
   = Unreachable
   | Nop
   | Block StackType [Expr]
   | Loop StackType [Expr]
   | If StackType [Expr] [Expr]
   | Br Var
   | BrTable [Var] Var
   | Return
   | Call Var
   | CallIndirect Var
   | Drop
   | Select
   | GetLocal Var
   | SetLocal Var
   | TeeLocal Var
   | GetGlobal Var
   | SetGlobal Var
   | Load LoadOp
   | Store StoreOp
   | CurrentMemory
   | GrowMemory
   | Const Lit
   | Test TestOp
   | Compare CompareOp
   | Unary UnaryOp
   | Binary BinaryOp
   | Convert ConvertOp
