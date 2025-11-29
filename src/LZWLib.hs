module LZWLib (lzwCompress, lzwDecompress) where

import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import Data.Char (chr, ord)
import Data.Maybe (fromMaybe)

-- Dictionary intializers

initialDict :: Map.Map String Int
initialDict = Map.fromList [ ([chr n], n) | n <- [0..255] ]

revInitialDict :: IntMap.IntMap String
revInitialDict = IntMap.fromList [ (n, [chr n]) | n <- [0..255] ]

-- compression algorithm

lzwCompress :: String -> [Int]
lzwCompress input = undefined
    
-- decompression algorithm

lzwDecompress :: [Int] -> Maybe String
lzwDecompress [] = Just ""
lzwDecompress (firstCode : rest) = undefined

-- helpers

