module Language.WebAssembly.AST where

import Data.Default.Class (Default(..))
import Data.Int           (Int32)
import Data.Maybe         (fromJust, isNothing)
import Data.Word          (Word32)
import Text.Printf        (printf)

import Language.WebAssembly.Operator 
import Language.WebAssembly.Type

data Position
   = Position
   { _posFile   :: FilePath
   , _posLine   :: Int
   , _posColumn :: Int
   } deriving (Eq)

instance Default Position where
   def = Position "" 0 0

instance Show Position where
   show Position {..} =
      if _posLine == -1
      then printf "0x%x" _posColumn
      else printf "%d.%d" _posLine $ succ _posColumn

data Region
   = Region
   { _regionLeft  :: Position
   , _regionRight :: Position
   } deriving (Eq)

instance Default Region where
   def = Region def def

instance Show Region where
   show Region {..} =
      printf "%s:%s" file area
      where file = _posFile _regionLeft
            area = show _regionLeft ++
               if _regionLeft == _regionRight
               then "" else "-" ++ show _regionRight

data Phrase a
   = Phrase
   { _phraseAt :: Region
   , _phraseIt :: a
   } deriving (Eq, Show)

infixl 5 @@
(@@) :: a -> Region -> Phrase a
(@@) = flip Phrase

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
