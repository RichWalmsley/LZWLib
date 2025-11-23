module LZWLib (compressLZW, decompressLZW) where

compressLZW :: Eq a => [a] -> [a] -> [Int]
compressLZW _ [] = []
compressLZW as (c:cs) = 

decompressLZW :: [a] -> [Int] -> [a] 
decompressLZW _ [] = []
decompressLZW as xs =
