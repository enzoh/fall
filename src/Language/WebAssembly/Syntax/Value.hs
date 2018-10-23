module Language.WebAssembly.Syntax.Value where

import Data.Bool (bool)
import Data.Word (Word32, Word64)

import Language.WebAssembly.Syntax.Type

data Value
   = I32 Word32
   | I64 Word64
   | F32 Float
   | F64 Double
   deriving (Show)

typeOf :: Value -> ValueType
typeOf = \ case
   I32 _ -> I32Type
   I64 _ -> I64Type
   F32 _ -> F32Type
   F64 _ -> F64Type

defaultValue :: ValueType -> Value
defaultValue = \ case
   I32Type -> I32 0
   I64Type -> I64 0
   F32Type -> F32 0
   F64Type -> F64 0

valueOfBool :: Bool -> Value
valueOfBool = I32 . bool 0 1
