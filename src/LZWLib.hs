module LZWLib (compressLZW, decompressLZW) where

-- compression algorithm

compressLZW :: Eq a => [a] -> [a] -> [Int]
compressLZW _ [] = []
compressLZW as (c:cs) = undefined 

-- compression helpers



-- decompression algorithm

decompressLZW :: [a] -> [Int] -> [a] 
decompressLZW _ [] = []
decompressLZW as xs = undefined

-- decompression helpers


