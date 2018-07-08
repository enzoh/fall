module Language.WebAssembly.Util.Source where

import Data.Default.Class (Default(..))
import Text.Printf (printf)

data Position
   = Position
   { _posFile :: FilePath
   , _posLine :: Int
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
   { _regionLeft :: Position
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
