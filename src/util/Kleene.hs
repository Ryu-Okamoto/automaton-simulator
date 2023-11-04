module Src.Util.Kleene where

import Data.Set.Monad ( Set, fromList, union )  
import Control.Monad ( replicateM )

import Src.Util.Synonym ( Symbol )

kleeneStar :: Int -> Set Symbol -> Set [Symbol]
kleeneStar 0 _ = fromList [""]
kleeneStar n alphabet = kleeneStar (n - 1) alphabet `union` replicateM n alphabet