module Main (main) where

import Data.Vector.Storable as Vector (Vector, fromList)
import Foreign.C.Types (CInt)
import Internal.Vectorized (stepI)

mat :: Vector CInt
mat = Vector.fromList [1,2,3,4,5,6,7,8,9,10,11,12]

main :: IO ()
main = print $ stepI mat
