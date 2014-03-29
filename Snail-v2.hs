module Snail (snail) where

type Matrix = [[Integer]]

inner :: [a] -> [a]
inner = tail . init

goRight :: Matrix -> [Integer]
goRight = head

goDown :: Matrix -> [Integer]
goDown = tail . map last

goLeft :: Matrix -> [Integer]
goLeft = tail . reverse . last

goUp :: Matrix -> [Integer]
goUp = reverse . init . tail . map head

perimeter :: Matrix -> [Integer]
perimeter x = goRight x ++ goDown x ++ goLeft x ++ goUp x

submatrix :: Matrix -> Matrix
submatrix = inner . map inner

snail :: Matrix -> [Integer]
snail [] = []
snail (x:[]) = x
snail ((x:[]):xs) = x : map head xs
snail x = perimeter x ++ snail ( submatrix x )
