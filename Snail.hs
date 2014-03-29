module Snail (snail) where

type Matrix = [[Integer]]

goRight :: Matrix -> [Integer]
goRight = head

goDown :: Matrix -> [Integer]
goDown x = tail $ map last x

rotate :: Matrix -> Matrix
rotate = reverse . map reverse

submatrix :: Matrix -> Matrix
submatrix = tail . map init

snail :: Matrix -> [Integer]
snail (x:[]) = x
snail x = goRight x ++ goDown x ++ snail ( rotate $ submatrix x )
