module LZWLib (lzwCompress, lzwDecompress) where

import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import Data.Char (chr, ord)
import Data.Maybe (fromMaybe)
import Data.Bits (Bits(xor))

-- Dictionary intializers

initialDict :: Map.Map String Int
initialDict = Map.fromList [ ([chr n], n) | n <- [0..255] ]

revInitialDict :: IntMap.IntMap String
revInitialDict = IntMap.fromList [ (n, [chr n]) | n <- [0..255] ]

-- compression algorithm

lzwCompress :: String -> [Int]
lzwCompress input = compressHelper input "" initialDict 256 []

compressHelper
    :: String -- remaining input
    -> String -- current prefix (w)
    -> Map.Map String Int -- dictionary
    -> Int -- next available code
    -> [Int] -- output codes reversed
    -> [Int] -- final list of codes 
compressHelper [] "" _ _ acc = reverse acc
compressHelper [] w dict _ acc = reverse (lookupCode dict w : acc)
compressHelper (k:ks) w dict nextCode acc = 
    let wk = w ++ [k]
    in if Map.member wk dict
        then compressHelper ks wk dict nextCode acc
        else
            let outCode = lookupCode dict w
                dict' = Map.insert wk nextCode dict
                acc' = outCode : acc
            in compressHelper ks [k] dict' (nextCode + 1) acc'
    
-- decompression algorithm

lzwDecompress :: [Int] -> Maybe String
lzwDecompress [] = Just ""
lzwDecompress (firstCode : rest) =
    case IntMap.lookup firstCode revInitialDict of
        Nothing -> Nothing
        Just firstStr ->
            Just (decompressHelper rest firstStr revInitialDict 256 firstStr)

decompressHelper
    :: [Int] -- remaining codes
    -> String -- previous string
    -> IntMap.IntMap String -- dictionary
    -> Int -- next code
    -> String -- output accumulated
    -> String -- final output
decompressHelper [] _ _ _ output = output
decompressHelper (code : codes) prev revDict nextCode output =
    let
        (entry, revDict', nextCode') =
            decodeEntry code prev revDict nextCode 
        output' = output ++ entry
    in decompressHelper codes entry revDict' nextCode' output'

-- helpers

lookupCode :: Map.Map String Int -> String -> Int
lookupCode dict s =
    case Map.lookup s dict of
        Just x -> x
        Nothing -> error ("lookupCode: missing key: " ++ show s)

decodeEntry
    :: Int -- current code
    -> String -- previous entry
    -> IntMap.IntMap String -- dictionary
    -> Int -- next code
    -> (String, IntMap.IntMap String, Int)
decodeEntry code prev dict nextCode =
    case IntMap.lookup code dict of
        Just entry ->
            -- regular case
            let newEntry = prev ++ [head entry]
                dict' = IntMap.insert nextCode newEntry dict
            in (entry, dict', nextCode + 1)
        Nothing ->
            -- special case: entry = prev ++ first(prev)
            let entry = prev ++ [head prev]
                dict' = IntMap.insert nextCode entry dict
            in (entry, dict', nextCode + 1)