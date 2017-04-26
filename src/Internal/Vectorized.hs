{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeOperators #-}
module Internal.Vectorized where

import Control.Monad (when)
import qualified Data.Vector.Storable as Vector (length)
import Data.Vector.Storable (Vector, unsafeFromForeignPtr, unsafeWith)
import Foreign.C.Types (CInt(..))
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import GHC.ForeignPtr (mallocPlainForeignPtrBytes)
import System.IO.Unsafe (unsafePerformIO)

infixr 1 #
a # b = avec a b
{-# INLINE (#) #-}

a #! b = a # b # id
{-# INLINE (#!) #-}

data FunCodeS = Norm2
              | AbsSum
              | MaxIdx
              | Max
              | MinIdx
              | Min
              deriving Enum

type TVV t = t :> t :> Ok

stepg f v = unsafePerformIO $ do
    r <- createVector (Vector.length v)
    (v #! r) f #|"step"
    return r

stepI :: Vector CInt -> Vector CInt
stepI = stepg c_stepI

foreign import ccall unsafe "stepI" c_stepI :: TVV CInt

-- GSL error codes are <= 1024
-- | error codes for the auxiliary functions required by the wrappers
errorCode :: CInt -> String
errorCode 2000 = "bad size"
errorCode 2001 = "bad function code"
errorCode 2002 = "memory problem"
errorCode 2003 = "bad file"
errorCode 2004 = "singular"
errorCode 2005 = "didn't converge"
errorCode 2006 = "the input matrix is not positive definite"
errorCode 2007 = "not yet supported in this OS"
errorCode n    = "code "++show n

-- | check the error code
check :: String -> IO CInt -> IO ()
check msg f = do
    err <- f
    when (err/=0) $ error (msg++": "++errorCode err)
    return ()

-- | postfix error code check
infixl 0 #|
(#|) :: IO CInt -> String -> IO ()
(#|) = flip check

type CV b r = CInt -> Ptr b -> r
type Ok = IO CInt

infixr 5 :>
type (:>)  t r = CV t r

{-# INLINE avec #-}
avec :: Storable a => Vector a -> (f -> IO r) -> ((CInt -> Ptr a -> f) -> IO r)
avec v f g = unsafeWith v $ \ptr -> f (g (fromIntegral (Vector.length v)) ptr)

-- allocates memory for a new vector
createVector :: Storable a => Int -> IO (Vector a)
createVector n = do
    when (n < 0) $ error ("trying to createVector of negative dim: "++show n)
    fp <- doMalloc undefined
    return $ unsafeFromForeignPtr fp 0 n
  where
    --
    -- Use the much cheaper Haskell heap allocated storage
    -- for foreign pointer space we control
    --
    doMalloc :: Storable b => b -> IO (ForeignPtr b)
    doMalloc dummy = do
        mallocPlainForeignPtrBytes (n * sizeOf dummy)
