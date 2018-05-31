module Language.WebAssembly.AST where

import Data.ByteString (ByteString)
import Data.Default.Class (Default(..))
import Data.Int (Int32)

import Language.WebAssembly.Operator
import Language.WebAssembly.Source
import Language.WebAssembly.Type
import Language.WebAssembly.Value

type Var = Phrase Int32

type Literal = Phrase Value

type Name = [Int]

type Instr = Phrase Instr'

data Instr'
   = Unreachable
   | Nop
   | Block StackType [Instr]
   | Loop StackType [Instr]
   | If StackType [Instr] [Instr]
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
   | Const Literal
   | Test TestOp
   | Compare CompareOp
   | Unary UnaryOp
   | Binary BinaryOp
   | Convert ConvertOp
   deriving (Show)

type Const = Phrase [Instr]

type Global = Phrase Global'

data Global'
   = Global'
   { _globalType :: GlobalType
   , _globalValue :: Const
   } deriving (Show)

type Func = Phrase Func'

data Func'
   = Func'
   { _funcType :: Var
   , _funcLocals :: [ValueType]
   , _funcBody :: [Instr]
   } deriving (Show)

type Table = Phrase Table'

data Table'
   = Table'
   { _tableType :: TableType
   } deriving (Show)

type Memory = Phrase Memory'

data Memory'
   = Memory'
   { _memoryType :: MemoryType
   } deriving (Show)

type Segment a = Phrase (Segment' a)

data Segment' a
   = Segment'
   { _segmentIndex :: Var
   , _segmentOffset :: Const
   , _segmentInit :: a
   } deriving (Show)

type TableSegment = Segment [Var]

type MemorySegment = Segment ByteString

type Type = Phrase FuncType

type ExportDesc = Phrase ExportDesc'

data ExportDesc'
   = FuncExport Var
   | TableExport Var
   | MemoryExport Var
   | GlobalExport Var
   deriving (Show)

type Export = Phrase Export'

data Export'
   = Export'
   { _exportName :: Name
   , _exportDesc :: ExportDesc
   } deriving (Show)

type ImportDesc = Phrase ImportDesc'

data ImportDesc'
   = FuncImport Var
   | TableImport TableType
   | MemoryImport MemoryType
   | GlobalImport GlobalType
   deriving (Show)

type Import = Phrase Import'

data Import'
   = Import'
   { _importModule :: Name
   , _importItem :: Name
   , _importDesc :: ImportDesc
   } deriving (Show)

type Module = Phrase Module'

data Module'
   = Module'
   { _moduleTypes :: [Type]
   , _moduleGlobals :: [Global]
   , _moduleTables :: [Table]
   , _moduleMemories :: [Memory]
   , _moduleFuncs :: [Func]
   , _moduleStart :: Maybe Var
   , _moduleElems :: [TableSegment]
   , _moduleData :: [MemorySegment]
   , _moduleImports :: [Import]
   , _moduleExports :: [Export]
   } deriving (Show)

instance Default Module' where
   def = Module' [] [] [] [] [] Nothing [] [] [] []
