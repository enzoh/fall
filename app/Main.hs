module Main where

import Data.Binary.Get (runGet)
import Data.ByteString.Lazy as Lazy (readFile)
import Data.Default.Class (Default(..))
import System.Console.CmdArgs (Data, cmdArgs)

import Language.WebAssembly.Binary.Decode

data Args
   = Args
   { file :: FilePath
   } deriving Data

instance Default Args where
   def = Args "a.wasm"

main :: IO ()
main = do
   Args {..} <- cmdArgs def
   bytes <- Lazy.readFile file
   let wasm = runGet getModule bytes
   print wasm
