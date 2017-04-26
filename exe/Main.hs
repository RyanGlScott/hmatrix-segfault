module Main (main) where

import Foreign.C.Types (CInt)
import Internal.Vectorized (Vector, fromList, stepI)

mat :: Vector CInt
mat = fromList [1,2,3,4,5,6,7,8,9,10,11,12]

main :: IO ()
main = print $ stepI mat
