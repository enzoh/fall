module Language.WebAssembly.Type where

import Data.Int (Int32)

data ValueType
   = I32Type
   | I64Type
   | F32Type
   | F64Type
   deriving (Show)

data ElemType
   = AnyFuncType
   deriving (Show)

type StackType = [] ValueType

data FuncType
   = FuncType StackType StackType
   deriving (Show)

data Limit a
   = Limit
   { _limLower :: a
   , _limUpper :: Maybe a
   } deriving (Show)

data Mutability
   = Immutable
   | Mutable
   deriving (Show)

data TableType
   = TableType (Limit Int32) ElemType
   deriving (Show)

data MemoryType
   = MemoryType (Limit Int32)
   deriving (Show)

data GlobalType
   = GlobalType ValueType Mutability
   deriving (Show)

data ExternType
   = ExternFuncType FuncType
   | ExternTableType TableType
   | ExternMemoryType MemoryType
   | ExternGlobalType GlobalType
   deriving (Show)
