module Language.WebAssembly.Binary.Decode where

import Data.Binary.Get (Get, getWord32be)
import Data.Binary.Get.Internal (bytesRead)
import Data.Word (Word32)
import Text.Printf (printf)

import Language.WebAssembly.Syntax.AST
import Language.WebAssembly.Util.Source

getPosition :: Get Position
getPosition = do
   column <- bytesRead
   return $ Position "" (-1) column

getPhrase :: Get a -> Get (Phrase a)
getPhrase getValue = do
   left <- getPosition
   value <- getValue
   right <- getPosition
   return $ value @@ Region left right

getModule' :: Get Module'
getModule' = do
   _ <- getMagic
   _ <- getVersion
   Module'
      <$> getTypes
      <*> getGlobals
      <*> getTables
      <*> getMemories
      <*> getFuncs
      <*> getStart
      <*> getElems
      <*> getData
      <*> getImports
      <*> getExports

getModule :: Get Module
getModule = getPhrase getModule'

getMagic :: Get Word32
getMagic = do
   magic <- getWord32be
   if magic == 0x0061736d
   then return magic
   else fail $ printf "getMagic: unknown magic: 0x%010x" magic

getVersion :: Get Word32
getVersion = do
   version <- getWord32be
   if version == 0x01000000
   then return version
   else fail $ printf "getVersion: unknown version: 0x%010x" version

getTypes :: Get [Type]
getTypes = undefined

getGlobals :: Get [Global]
getGlobals = undefined

getTables :: Get [Table]
getTables = undefined

getMemories :: Get [Memory]
getMemories = undefined

getFuncs :: Get [Func]
getFuncs = undefined

getStart :: Get (Maybe Var)
getStart = undefined

getElems :: Get [TableSegment]
getElems = undefined

getData :: Get [MemorySegment]
getData = undefined

getImports :: Get [Import]
getImports = undefined

getExports :: Get [Export]
getExports = undefined
