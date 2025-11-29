module LZWLib (lzwCompress, lzwDecompress) where

import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import Data.Char (chr, ord)
import Data.Maybe (fromMaybe)


initalDict :: Map.Map String Int
initialDict = Map.fromList [ ([chr n], n) | n <- [0..255] ]

revInitialDict :: IntMap.IntMap String
revInittialDict = IntMap.fromList [ (n, [chr n]) | n <- [0..255] ]

-- compression algorithm

compressLZW :: String -> [Int]
compressLZW _ [] = []
compressLZW input = compressLoop input "" initialDict 256 []
    where
        compressLoop
            :: String               -- remaining input
            -> String               -- current prefix (w)
            -> Map.Map String Int   -- dict
            -> Int                  -- next available code
            -> [Int]                -- output codes reversed
            -> [Int]                -- final code list
        compressLoop [] "" _ _ acc = reverse acc
        compressLoop [] w dict _ acc = reverse (lookupCode dict w : acc)
        compressLoop (k:ks) w dict nextCode acc =
            let wk = w ++ [k]
            in if Map.member wk dict
                then compressLoop ks wk dict nextCode acc
                else
                    let outCode = lookupCode dict w
                        dict'   = Map.insert wk nextCode dict
                        acc'    = outCode : acc
                    in compressLoop ks [k] dict' (nextCodde + 1) acc'

-- decompression algorithm

lzwDecompress :: [Int] -> Maybe String
lzwDecompress [] = Just ""
lzwDecompress (firstCode : rest) = undefined

-- Utility

lookupCode :: Map.Map String Int -> String -> Int
lookupCode = undefined
