module Language.WebAssembly.Operator where

import Language.WebAssembly.Operator.Float
import Language.WebAssembly.Operator.Generic
import Language.WebAssembly.Operator.Int

data UnaryOp
   = I32UnaryOp (I32Op Unary)
   | I64UnaryOp (I64Op Unary)
   | F32UnaryOp (F32Op Unary)
   | F64UnaryOp (F64Op Unary)
   deriving (Show)

data BinaryOp
   = I32BinaryOp (I32Op Binary)
   | I64BinaryOp (I64Op Binary)
   | F32BinaryOp (F32Op Binary)
   | F64BinaryOp (F64Op Binary)
   deriving (Show)

data TestOp
   = I32TestOp (I32Op Test)
   | I64TestOp (I64Op Test)
   deriving (Show)

data CompareOp
   = I32CompareOp (I32Op Compare)
   | I64CompareOp (I64Op Compare)
   | F32CompareOp (F32Op Compare)
   | F64CompareOp (F64Op Compare)
   deriving (Show)

data ConvertOp
   = I32ConvertOp (I32Op Convert)
   | I64ConvertOp (I64Op Convert)
   | F32ConvertOp (F32Op Convert)
   | F64ConvertOp (F64Op Convert)
   deriving (Show)

data LoadOp
   = LoadOp
   deriving (Show)

data StoreOp
   = StoreOp
   deriving (Show)
