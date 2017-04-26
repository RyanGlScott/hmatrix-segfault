{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
module Internal.Vectorized where

import Control.Monad (when)
-- import qualified Data.Vector.Storable as Vector (length)
-- import Data.Vector.Storable (Vector, unsafeFromForeignPtr, unsafeWith)
import Foreign.C.Types (CInt(..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Array (advancePtr)
import Foreign.Storable (Storable(..))
import GHC.Exts (RealWorld, State#, build, realWorld#, unsafeCoerce#)
import GHC.ForeignPtr (ForeignPtr(..), mallocPlainForeignPtrBytes)
import GHC.IO (IO(..))
import GHC.Ptr (Ptr(..))
import GHC.ST (ST(..), runST)
import GHC.Types (SPEC(..))
import System.IO.Unsafe (unsafePerformIO)

infixr 1 #
a # b = avec a b
{-# INLINE ( # ) #-}

a #! b = a # b # id
{-# INLINE ( #! ) #-}

data FunCodeS = Norm2
              | AbsSum
              | MaxIdx
              | Max2
              | MinIdx
              | Min
              deriving Enum

type TVV t = t :> t :> Ok

stepg f v = unsafePerformIO $ do
    r <- createVector (vlength v)
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
( #| ) :: IO CInt -> String -> IO ()
( #| ) = flip check

type CV b r = CInt -> Ptr b -> r
type Ok = IO CInt

infixr 5 :>
type (:>)  t r = CV t r

{-# INLINE avec #-}
avec :: Storable a => Vector a -> (f -> IO r) -> ((CInt -> Ptr a -> f) -> IO r)
avec v f g = unsafeWith v $ \ptr -> f (g (fromIntegral (vlength v)) ptr)

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

-------------------------------------------------------------------------------
-- vector junk
-------------------------------------------------------------------------------

#define PHASE_FUSED [1]
#define PHASE_INNER [0]

#define INLINE_FUSED INLINE PHASE_FUSED
#define INLINE_INNER INLINE PHASE_INNER

class Monad m => PrimMonad m where
  type PrimState m
  primitive :: (State# (PrimState m) -> (# State# (PrimState m), a #)) -> m a

class PrimMonad m => PrimBase m where
  internal :: m a -> State# (PrimState m) -> (# State# (PrimState m), a #)

instance PrimMonad (ST s) where
  type PrimState (ST s) = s
  primitive = ST
  {-# INLINE primitive #-}

instance PrimMonad IO where
  type PrimState IO = RealWorld
  primitive = IO
  {-# INLINE primitive #-}
instance PrimBase IO where
  internal (IO p) = p
  {-# INLINE internal #-}

unsafePrimToPrim :: (PrimBase m1, PrimMonad m2) => m1 a -> m2 a
{-# INLINE unsafePrimToPrim #-}
unsafePrimToPrim m = primitive (unsafeCoerce# (internal m))

unsafeInlineIO :: IO a -> a
{-# INLINE unsafeInlineIO #-}
unsafeInlineIO m = case internal m realWorld# of (# _, r #) -> r

class GMVector v a where
  gmbasicLength       :: v s a -> Int

  gmbasicUnsafeSlice :: Int  -- ^ starting index
                   -> Int  -- ^ length of the slice
                   -> v s a
                   -> v s a

  gmbasicOverlaps    :: v s a -> v s a -> Bool

  gmbasicUnsafeNew   :: PrimMonad m => Int -> m (v (PrimState m) a)

  gmbasicInitialize :: PrimMonad m => v (PrimState m) a -> m ()

  gmbasicUnsafeReplicate :: PrimMonad m => Int -> a -> m (v (PrimState m) a)

  gmbasicUnsafeRead  :: PrimMonad m => v (PrimState m) a -> Int -> m a

  gmbasicUnsafeWrite :: PrimMonad m => v (PrimState m) a -> Int -> a -> m ()

  gmbasicClear       :: PrimMonad m => v (PrimState m) a -> m ()

  gmbasicSet         :: PrimMonad m => v (PrimState m) a -> a -> m ()

  gmbasicUnsafeCopy  :: PrimMonad m => v (PrimState m) a   -- ^ target
                                  -> v (PrimState m) a   -- ^ source
                                  -> m ()

  gmbasicUnsafeMove  :: PrimMonad m => v (PrimState m) a   -- ^ target
                                  -> v (PrimState m) a   -- ^ source
                                  -> m ()

  gmbasicUnsafeGrow  :: PrimMonad m => v (PrimState m) a -> Int
                                                       -> m (v (PrimState m) a)

  {-# INLINE gmbasicUnsafeReplicate #-}
  gmbasicUnsafeReplicate n x
    = do
        v <- gmbasicUnsafeNew n
        gmbasicSet v x
        return v

  {-# INLINE gmbasicClear #-}
  gmbasicClear _ = return ()

  {-# INLINE gmbasicSet #-}
  gmbasicSet !v x
    | n == 0    = return ()
    | otherwise = do
                    gmbasicUnsafeWrite v 0 x
                    do_set 1
    where
      !n = gmbasicLength v

      do_set i | 2*i < n = do gmbasicUnsafeCopy (gmbasicUnsafeSlice i i v)
                                              (gmbasicUnsafeSlice 0 i v)
                              do_set (2*i)
               | otherwise = gmbasicUnsafeCopy (gmbasicUnsafeSlice i (n-i) v)
                                             (gmbasicUnsafeSlice 0 (n-i) v)

  {-# INLINE gmbasicUnsafeCopy #-}
  gmbasicUnsafeCopy !dst !src = do_copy 0
    where
      !n = gmbasicLength src

      do_copy i | i < n = do
                            x <- gmbasicUnsafeRead src i
                            gmbasicUnsafeWrite dst i x
                            do_copy (i+1)
                | otherwise = return ()

  {-# INLINE gmbasicUnsafeMove #-}
  gmbasicUnsafeMove !dst !src
    | gmbasicOverlaps dst src = do
        srcCopy <- gmbasicUnsafeNew (gmbasicLength src)
        gmbasicUnsafeCopy srcCopy src
        gmbasicUnsafeCopy dst srcCopy
    | otherwise = gmbasicUnsafeCopy dst src

  {-# INLINE gmbasicUnsafeGrow #-}
  gmbasicUnsafeGrow v by
    = do
        v' <- gmbasicUnsafeNew (n+by)
        gmbasicUnsafeCopy (gmbasicUnsafeSlice 0 n v') v
        return v'
    where
      n = gmbasicLength v

type family GMutable (v :: * -> *) :: * -> * -> *

class GMVector (GMutable v) a => GVector v a where
  gbasicUnsafeFreeze :: PrimMonad m => GMutable v (PrimState m) a -> m (v a)

  gbasicUnsafeThaw :: PrimMonad m => v a -> m (GMutable v (PrimState m) a)

  gbasicLength      :: v a -> Int

  gbasicUnsafeSlice  :: Int -- ^ starting index
                    -> Int -- ^ length
                    -> v a -> v a

  gbasicUnsafeIndexM  :: Monad m => v a -> Int -> m a

  gbasicUnsafeCopy :: PrimMonad m => GMutable v (PrimState m) a -> v a -> m ()

  {-# INLINE gbasicUnsafeCopy #-}
  gbasicUnsafeCopy !dst !src = do_copy 0
    where
      !n = gbasicLength src

      do_copy i | i < n = do
                            x <- gbasicUnsafeIndexM src i
                            gmbasicUnsafeWrite dst i x
                            do_copy (i+1)
                | otherwise = return ()

  gelemseq :: v a -> a -> b -> b

  {-# INLINE gelemseq #-}
  gelemseq _ = \_ x -> x

delay_inline :: (a -> b) -> a -> b
{-# INLINE [0] delay_inline #-}
delay_inline f = f

data Step s a where
  Yield :: a -> s -> Step s a
  Skip  :: s -> Step s a
  Done  :: Step s a

instance Functor (Step s) where
  {-# INLINE fmap #-}
  fmap f (Yield x s) = Yield (f x) s
  fmap _ (Skip s) = Skip s
  fmap _ Done = Done

data Stream m a = forall s. Stream (s -> m (Step s a)) s

data Chunk v a = Chunk Int (forall m. (PrimMonad m, GVector v a) => GMutable v (PrimState m) a -> m ())

data Size = Exact Int          -- ^ Exact size
          | Max   Int          -- ^ Upper bound on the size
          | Unknown            -- ^ Unknown size

data MBundle m v a = MBundle { sElems  :: Stream m a
                             , sChunks :: Stream m (Chunk v a)
                             , sVector :: Maybe (v a)
                             , sSize   :: Size
                             }

newtype Id a = Id { unId :: a }

instance Functor Id where
  fmap f (Id x) = Id (f x)

instance Applicative Id where
  pure = Id
  Id f <*> Id x = Id (f x)

instance Monad Id where
  return     = Id
  Id x >>= f = f x

data Box a = Box { unBox :: a }

instance Functor Box where
  fmap f (Box x) = Box (f x)

instance Applicative Box where
  pure = Box
  Box f <*> Box x = Box (f x)

instance Monad Box where
  return      = Box
  Box x >>= f = f x

type Bundle = MBundle Id

data New v a = New (forall s. ST s (GMutable v s a))

upperBound :: Size -> Maybe Int
upperBound (Exact n) = Just n
upperBound (Max   n) = Just n
upperBound Unknown   = Nothing

mbfromList :: Monad m => [a] -> MBundle m v a
{-# INLINE mbfromList #-}
mbfromList xs = mbunsafeFromList Unknown xs

mbfromStream :: Monad m => Stream m a -> Size -> MBundle m v a
{-# INLINE mbfromStream #-}
mbfromStream (Stream step t) sz = MBundle (Stream step t) (Stream step' t) Nothing sz
  where
    step' s = do r <- step s
                 return $ fmap (\x -> Chunk 1 (\v -> gmbasicUnsafeWrite v 0 x)) r


mbfromVector :: (Monad m, GVector v a) => v a -> MBundle m v a
{-# INLINE_FUSED mbfromVector #-}
mbfromVector v = v `seq` n `seq` MBundle (Stream step 0)
                                      (Stream vstep True)
                                      (Just v)
                                      (Exact n)
  where
    n = gbasicLength v

    {-# INLINE step #-}
    step i | i >= n = return Done
           | otherwise = case gbasicUnsafeIndexM v i of
                           Box x -> return $ Yield x (i+1)


    {-# INLINE vstep #-}
    vstep True  = return (Yield (Chunk (gbasicLength v) (\mv -> gbasicUnsafeCopy mv v)) False)
    vstep False = return Done

mblength :: Monad m => MBundle m v a -> m Int
{-# INLINE_FUSED mblength #-}
mblength MBundle{sSize = Exact n}  = return n
mblength MBundle{sChunks = s} = sfoldl' (\n (Chunk k _) -> n+k) 0 s

mbunsafeFromList :: Monad m => Size -> [a] -> MBundle m v a
{-# INLINE_FUSED mbunsafeFromList #-}
mbunsafeFromList sz xs = mbfromStream (sfromList xs) sz

bchunks :: MBundle m v a -> Stream m (Chunk v a)
{-# INLINE bchunks #-}
bchunks = sChunks

bfromList :: [a] -> Bundle v a
{-# INLINE bfromList #-}
bfromList = mbfromList

bfromVector :: GVector v a => v a -> Bundle v a
{-# INLINE bfromVector #-}
bfromVector = mbfromVector

blength :: Bundle v a -> Int
{-# INLINE blength #-}
blength = unId . mblength

bsize :: MBundle m v a -> Size
{-# INLINE bsize #-}
bsize = sSize

blift :: Monad m => Bundle v a -> MBundle m v a
{-# INLINE_FUSED blift #-}
blift (MBundle (Stream step s) (Stream vstep t) v sz)
     = MBundle (Stream (return . unId . step) s)
               (Stream (return . unId . vstep) t) v sz

btoList :: Bundle v a -> [a]
{-# INLINE btoList #-}
btoList s = build (\c n -> btoListFB c n s)

btoListFB :: (a -> b -> b) -> b -> Bundle v a -> b
{-# INLINE [0] btoListFB #-}
btoListFB c n MBundle{sElems = Stream step t} = go t
  where
    go s = case unId (step s) of
             Yield x s' -> x `c` go s'
             Skip    s' -> go s'
             Done       -> n

sfromList :: Monad m => [a] -> Stream m a
{-# INLINE sfromList #-}
sfromList zs = Stream step zs
  where
    step (x:xs) = return (Yield x xs)
    step []     = return Done

sfoldlM :: Monad m => (a -> b -> m a) -> a -> Stream m b -> m a
{-# INLINE_FUSED sfoldlM #-}
sfoldlM m w (Stream step t) = foldlM_loop SPEC w t
  where
    foldlM_loop !_ z s
      = do
          r <- step s
          case r of
            Yield x s' -> do { z' <- m z x; foldlM_loop SPEC z' s' }
            Skip    s' -> foldlM_loop SPEC z s'
            Done       -> return z

sfoldl' :: Monad m => (a -> b -> a) -> a -> Stream m b -> m a
{-# INLINE sfoldl' #-}
sfoldl' f = sfoldlM' (\a b -> return (f a b))

sfoldlM' :: Monad m => (a -> b -> m a) -> a -> Stream m b -> m a
{-# INLINE_FUSED sfoldlM' #-}
sfoldlM' m w (Stream step t) = foldlM'_loop SPEC w t
  where
    foldlM'_loop !_ z s
      = z `seq`
        do
          r <- step s
          case r of
            Yield x s' -> do { z' <- m z x; foldlM'_loop SPEC z' s' }
            Skip    s' -> foldlM'_loop SPEC z s'
            Done       -> return z

gmlength :: GMVector v a => v s a -> Int
{-# INLINE gmlength #-}
gmlength = gmbasicLength

gmenlarge_delta :: GMVector v a => v s a -> Int
gmenlarge_delta v = max (gmlength v) 1

gmunsafeNew :: (PrimMonad m, GMVector v a) => Int -> m (v (PrimState m) a)
{-# INLINE gmunsafeNew #-}
gmunsafeNew n = {-UNSAFE_CHECK(checkLength) "unsafeNew" n
            $-} gmbasicUnsafeNew n

gmunsafeGrow :: (PrimMonad m, GMVector v a)
                        => v (PrimState m) a -> Int -> m (v (PrimState m) a)
{-# INLINE gmunsafeGrow #-}
gmunsafeGrow v n = {-UNSAFE_CHECK(checkLength) "unsafeGrow" n
               $-} gmbasicUnsafeGrow v n

gmunsafeSlice :: GMVector v a => Int  -- ^ starting index
                           -> Int  -- ^ length of the slice
                           -> v s a
                           -> v s a
{-# INLINE gmunsafeSlice #-}
gmunsafeSlice i n v = {-UNSAFE_CHECK(checkSlice) "unsafeSlice" i n (length v)
                  $-} gmbasicUnsafeSlice i n v

gmvunstream :: (PrimMonad m, GVector v a)
         => Bundle v a -> m (GMutable v (PrimState m) a)
{-# INLINE_FUSED gmvunstream #-}
gmvunstream s = gmvmunstream (blift s)

gmvmunstream :: (PrimMonad m, GVector v a)
           => MBundle m v a -> m (GMutable v (PrimState m) a)
{-# INLINE_FUSED gmvmunstream #-}
gmvmunstream s = case upperBound (bsize s) of
                 Just n  -> gmvmunstreamMax     s n
                 Nothing -> gmvmunstreamUnknown s

gmvmunstreamMax :: (PrimMonad m, GVector v a)
              => MBundle m v a -> Int -> m (GMutable v (PrimState m) a)
{-# INLINE gmvmunstreamMax #-}
gmvmunstreamMax s n
  = do
      v <- {-INTERNAL_CHECK(checkLength) "munstreamMax" n
           $-} gmunsafeNew n
      let {-# INLINE_INNER copyChunk #-}
          copyChunk i (Chunk m f) =
            {-INTERNAL_CHECK(checkSlice) "munstreamMax.copyChunk" i m (length v) $-} do
              f (gmbasicUnsafeSlice i m v)
              return (i+m)

      n' <- sfoldlM' copyChunk 0 (bchunks s)
      return -- $ INTERNAL_CHECK(checkSlice) "munstreamMax" 0 n' n
             $ gmunsafeSlice 0 n' v

gmvmunstreamUnknown :: (PrimMonad m, GVector v a)
                 => MBundle m v a -> m (GMutable v (PrimState m) a)
{-# INLINE gmvmunstreamUnknown #-}
gmvmunstreamUnknown s
  = do
      v <- gmunsafeNew 0
      (v', n) <- sfoldlM copyChunk (v,0) (bchunks s)
      return -- $ INTERNAL_CHECK(checkSlice) "munstreamUnknown" 0 n (length v')
             $ gmunsafeSlice 0 n v'
  where
    {-# INLINE_INNER copyChunk #-}
    copyChunk (v,i) (Chunk n f)
      = do
          let j = i+n
          v' <- if gmbasicLength v < j
                  then gmunsafeGrow v (delay_inline max (gmenlarge_delta v) (j - gmbasicLength v))
                  else return v
          -- INTERNAL_CHECK(checkSlice) "munstreamUnknown.copyChunk" i n (length v')
          f (gmbasicUnsafeSlice i n v')
          return (v',j)

newrun :: New v a -> ST s (GMutable v s a)
{-# INLINE newrun #-}
newrun (New p) = p

newunstream :: GVector v a => Bundle v a -> New v a
{-# INLINE_FUSED newunstream #-}
newunstream s = s `seq` New (gmvunstream s)

gunsafeFreeze
  :: (PrimMonad m, GVector v a) => GMutable v (PrimState m) a -> m (v a)
{-# INLINE gunsafeFreeze #-}
gunsafeFreeze = gbasicUnsafeFreeze

gnew :: GVector v a => New v a -> v a
{-# INLINE_FUSED gnew #-}
gnew m = m `seq` runST (gunsafeFreeze =<< newrun m)

gunstream :: GVector v a => Bundle v a -> v a
{-# INLINE gunstream #-}
gunstream s = gnew (newunstream s)

gfromList :: GVector v a => [a] -> v a
{-# INLINE gfromList #-}
gfromList = gunstream . bfromList

glength :: GVector v a => v a -> Int
{-# INLINE glength #-}
glength = blength . gstream

gshowsPrec :: (GVector v a, Show a) => Int -> v a -> ShowS
{-# INLINE gshowsPrec #-}
gshowsPrec _ = shows . gtoList

gstream :: GVector v a => v a -> Bundle v a
{-# INLINE_FUSED gstream #-}
gstream v = bfromVector v

gtoList :: GVector v a => v a -> [a]
{-# INLINE gtoList #-}
gtoList = btoList . gstream

fromList :: Storable a => [a] -> Vector a
{-# INLINE fromList #-}
fromList = gfromList

vlength :: Storable a => Vector a -> Int
{-# INLINE vlength #-}
vlength = glength

data Vector a = Vector {-# UNPACK #-} !Int
                       {-# UNPACK #-} !(ForeignPtr a)

data MVector s a = MVector {-# UNPACK #-} !Int
                           {-# UNPACK #-} !(ForeignPtr a)

type instance GMutable Vector = MVector

instance (Show a, Storable a) => Show (Vector a) where
  showsPrec = gshowsPrec

instance Storable a => GMVector MVector a where
  {-# INLINE gmbasicLength #-}
  gmbasicLength (MVector n _) = n

  {-# INLINE gmbasicUnsafeSlice #-}
  gmbasicUnsafeSlice j m (MVector _ fp) = MVector m (updPtr (`advancePtr` j) fp)

{-
  -- FIXME: this relies on non-portable pointer comparisons
  {-# INLINE gmbasicOverlaps #-}
  gmbasicOverlaps (MVector m fp) (MVector n fq)
    = between p q (q `advancePtr` n) || between q p (p `advancePtr` m)
    where
      between x y z = x >= y && x < z
      p = getPtr fp
      q = getPtr fq
-}

  {-# INLINE gmbasicUnsafeNew #-}
  gmbasicUnsafeNew n
    | n < 0 = error $ "Storable.basicUnsafeNew: negative length: " ++ show n
    | n > mx = error $ "Storable.basicUnsafeNew: length too large: " ++ show n
    | otherwise = unsafePrimToPrim $ do
        fp <- mallocVector n
        return $ MVector n fp
    where
      size = sizeOf (undefined :: a)
      mx = maxBound `quot` size :: Int

{-
  {-# INLINE gmbasicInitialize #-}
  gmbasicInitialize = storableZero
-}

  {-# INLINE gmbasicUnsafeRead #-}
  gmbasicUnsafeRead (MVector _ fp) i
    = unsafePrimToPrim
    $ withForeignPtr fp (`peekElemOff` i)

  {-# INLINE gmbasicUnsafeWrite #-}
  gmbasicUnsafeWrite (MVector _ fp) i x
    = unsafePrimToPrim
    $ withForeignPtr fp $ \p -> pokeElemOff p i x

{-
  {-# INLINE gmbasicSet #-}
  gmbasicSet = storableSet

  {-# INLINE gmbasicUnsafeCopy #-}
  gmbasicUnsafeCopy (MVector n fp) (MVector _ fq)
    = unsafePrimToPrim
    $ withForeignPtr fp $ \p ->
      withForeignPtr fq $ \q ->
      copyArray p q n

  {-# INLINE gmbasicUnsafeMove #-}
  gmbasicUnsafeMove (MVector n fp) (MVector _ fq)
    = unsafePrimToPrim
    $ withForeignPtr fp $ \p ->
      withForeignPtr fq $ \q ->
      moveArray p q n
-}

instance Storable a => GVector Vector a where
  {-# INLINE gbasicUnsafeFreeze #-}
  gbasicUnsafeFreeze (MVector n fp) = return $ Vector n fp

{-
  {-# INLINE gbasicUnsafeThaw #-}
  gbasicUnsafeThaw (Vector n fp) = return $ MVector n fp
-}

  {-# INLINE gbasicLength #-}
  gbasicLength (Vector n _) = n

{-
  {-# INLINE gbasicUnsafeSlice #-}
  gbasicUnsafeSlice i n (Vector _ fp) = Vector n (updPtr (`advancePtr` i) fp)
-}

  {-# INLINE gbasicUnsafeIndexM #-}
  gbasicUnsafeIndexM (Vector _ fp) i = return
                                     . unsafeInlineIO
                                     $ withForeignPtr fp $ \p ->
                                       peekElemOff p i

{-
  {-# INLINE gbasicUnsafeCopy #-}
  gbasicUnsafeCopy (MVector n fp) (Vector _ fq)
    = unsafePrimToPrim
    $ withForeignPtr fp $ \p ->
      withForeignPtr fq $ \q ->
      copyArray p q n

  {-# INLINE gelemseq #-}
  gelemseq _ = seq
-}

unsafeWith :: Storable a => Vector a -> (Ptr a -> IO b) -> IO b
{-# INLINE unsafeWith #-}
unsafeWith (Vector _ fp) = withForeignPtr fp

unsafeFromForeignPtr :: Storable a
                     => ForeignPtr a    -- ^ pointer
                     -> Int             -- ^ offset
                     -> Int             -- ^ length
                     -> Vector a
{-# INLINE_FUSED unsafeFromForeignPtr #-}
unsafeFromForeignPtr fp i n = unsafeFromForeignPtr0 fp' n
    where
      fp' = updPtr (`advancePtr` i) fp

{-# RULES
"unsafeFromForeignPtr fp 0 n -> unsafeFromForeignPtr0 fp n " forall fp n.
  unsafeFromForeignPtr fp 0 n = unsafeFromForeignPtr0 fp n   #-}

unsafeFromForeignPtr0 :: Storable a
                      => ForeignPtr a    -- ^ pointer
                      -> Int             -- ^ length
                      -> Vector a
{-# INLINE unsafeFromForeignPtr0 #-}
unsafeFromForeignPtr0 fp n = Vector n fp

updPtr :: (Ptr a -> Ptr a) -> ForeignPtr a -> ForeignPtr a
{-# INLINE updPtr #-}
updPtr f (ForeignPtr p c) = case f (Ptr p) of { Ptr q -> ForeignPtr q c }

{-# INLINE mallocVector #-}
mallocVector :: Storable a => Int -> IO (ForeignPtr a)
mallocVector =
    doMalloc undefined
        where
          doMalloc :: Storable b => b -> Int -> IO (ForeignPtr b)
          doMalloc dummy size = mallocPlainForeignPtrBytes (size * sizeOf dummy)
