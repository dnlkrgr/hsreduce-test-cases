{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Bug (gcons) where

import Control.Monad.ST (runST)
import Data.Kind (Type)
import Data.Functor.Identity (Identity(..))
import GHC.ST (ST(..))
import GHC.Types (SPEC(..))

gcons :: forall v a. GVector v a => a -> v a -> v a
gcons x v = gelemseq (undefined :: v a) x
          $ gunstream
          $ bcons x
          $ gstream v
{-# INLINE gcons #-}

-----

delay_inline :: (a -> b) -> a -> b
delay_inline f = f
{-# INLINE [0] delay_inline #-}

data New v a = New (forall s. ST s (Mutable v s a))

data Chunk v a = Chunk Int (forall m. (PrimMonad m, GVector v a) => Mutable v (PrimState m) a -> m ())

data Box a = Box a
instance Functor Box where
  fmap f (Box x) = Box (f x)
instance Applicative Box where
  pure = Box
  Box f <*> Box x = Box (f x)
instance Monad Box where
  return = pure
  Box x >>= f = f x

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

data Size = Exact Int
          | Max   Int
          | Unknown

instance Num Size where
  Exact m + Exact n = checkedAdd Exact m n
  Exact m + Max   n = checkedAdd Max m n

  Max   m + Exact n = checkedAdd Max m n
  Max   m + Max   n = checkedAdd Max m n

  _       + _       = Unknown


  Exact m - Exact n = checkedSubtract Exact m n
  Exact m - Max   _ = Max   m

  Max   m - Exact n = checkedSubtract Max m n
  Max   m - Max   _ = Max   m
  Max   m - Unknown = Max   m

  _       - _       = Unknown


  fromInteger n     = Exact (fromInteger n)

  (*)    = error "vector: internal error * for Bundle.size isn't defined"
  abs    = error "vector: internal error abs for Bundle.size isn't defined"
  signum = error "vector: internal error signum for Bundle.size isn't defined"

checkedAdd :: (Int -> Size) -> Int -> Int -> Size
checkedAdd con m n
    -- Note: we assume m and n are >= 0.
  | r < m || r < n =
      error $ "Data.Vector.Fusion.Bundle.Size.checkedAdd: overflow: " ++ show r
  | otherwise = con r
  where
    r = m + n
{-# INLINE checkedAdd #-}

checkedSubtract :: (Int -> Size) -> Int -> Int -> Size
checkedSubtract con m n
  | r < 0 =
      error $ "Data.Vector.Fusion.Bundle.Size.checkedSubtract: underflow: " ++ show r
  | otherwise = con r
  where
    r = m - n
{-# INLINE checkedSubtract #-}

upperBound :: Size -> Maybe Int
upperBound (Exact n) = Just n
upperBound (Max   n) = Just n
upperBound Unknown   = Nothing

type Bundle = MBundle Identity

data MBundle m v a = Bundle { _sElems  :: Stream m a
                            , sChunks  :: Stream m (Chunk v a)
                            , _sVector :: Maybe (v a)
                            , sSize    :: Size
                            }

class Monad m => PrimMonad m where
  type PrimState m
instance PrimMonad (ST s) where
  type PrimState (ST s) = s

type family Mutable (v :: Type -> Type) :: Type -> Type -> Type

class GMVector v a where
  gmbasicLength      :: v s a -> Int
  gmbasicUnsafeSlice :: Int
                     -> Int
                     -> v s a
                     -> v s a
  gmbasicUnsafeNew   :: PrimMonad m => Int -> m (v (PrimState m) a)
  gmbasicUnsafeWrite :: PrimMonad m => v (PrimState m) a -> Int -> a -> m ()
  gmbasicUnsafeGrow  :: PrimMonad m => v (PrimState m) a -> Int
                                                         -> m (v (PrimState m) a)

class GMVector (Mutable v) a => GVector v a where
  gbasicUnsafeFreeze :: PrimMonad m => Mutable v (PrimState m) a -> m (v a)
  gbasicLength       :: v a -> Int
  gbasicUnsafeIndexM :: Monad m => v a -> Int -> m a
  gbasicUnsafeCopy   :: PrimMonad m => Mutable v (PrimState m) a -> v a -> m ()
  gelemseq           :: v a -> a -> b -> b

sfoldlM :: Monad m => (a -> b -> m a) -> a -> Stream m b -> m a
sfoldlM m w (Stream step t) = foldlM_loop SPEC w t
  where
    foldlM_loop !_ z s
      = do
          r <- step s
          case r of
            Yield x s' -> do { z' <- m z x; foldlM_loop SPEC z' s' }
            Skip    s' -> foldlM_loop SPEC z s'
            Done       -> return z
{-# INLINE [1] sfoldlM #-}

sfoldlM' :: Monad m => (a -> b -> m a) -> a -> Stream m b -> m a
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
{-# INLINE [1] sfoldlM' #-}

sappend :: Monad m => Stream m a -> Stream m a -> Stream m a
Stream stepa ta `sappend` Stream stepb tb = Stream step (Left ta)
  where
    {-# INLINE [0] step #-}
    step (Left  sa) = do
                        r <- stepa sa
                        case r of
                          Yield x sa' -> return $ Yield x (Left  sa')
                          Skip    sa' -> return $ Skip    (Left  sa')
                          Done        -> return $ Skip    (Right tb)
    step (Right sb) = do
                        r <- stepb sb
                        case r of
                          Yield x sb' -> return $ Yield x (Right sb')
                          Skip    sb' -> return $ Skip    (Right sb')
                          Done        -> return $ Done
{-# INLINE [1] sappend #-}

ssingleton :: Monad m => a -> Stream m a
ssingleton x = Stream (return . step) True
  where
    {-# INLINE [0] step #-}
    step True  = Yield x False
    step False = Done
{-# INLINE [1] ssingleton #-}

bmappend :: Monad m => MBundle m v a -> MBundle m v a -> MBundle m v a
Bundle sa ta _ na `bmappend` Bundle sb tb _ nb =
  Bundle (sappend sa sb) (sappend ta tb) Nothing (na + nb)
{-# INLINE [1] bmappend #-}

bfromVector :: GVector v a => v a -> Bundle v a
bfromVector = bmfromVector
{-# INLINE bfromVector #-}

blift :: Monad m => Bundle v a -> MBundle m v a
blift (Bundle (Stream step s) (Stream vstep t) v sz)
    = Bundle (Stream (return . runIdentity . step) s)
             (Stream (return . runIdentity . vstep) t) v sz
{-# INLINE [1] blift #-}

bmfromVector :: (Monad m, GVector v a) => v a -> MBundle m v a
bmfromVector v = v `seq` n `seq` Bundle (Stream step 0)
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

bcons :: a -> Bundle v a -> Bundle v a
bcons = bmcons
{-# INLINE bcons #-}

bmcons :: Monad m => a -> MBundle m v a -> MBundle m v a
bmcons x s = bmappend (bmsingleton x) s
{-# INLINE bmcons #-}

bmsingleton :: Monad m => a -> MBundle m v a
bmsingleton x = bmfromStream (ssingleton x) (Exact 1)
{-# INLINE [1] bmsingleton #-}

bmfromStream :: Monad m => Stream m a -> Size -> MBundle m v a
bmfromStream (Stream step t) sz = Bundle (Stream step t) (Stream step' t) Nothing sz
  where
    step' s = do r <- step s
                 return $ fmap (\x -> Chunk 1 (\v -> gmbasicUnsafeWrite v 0 x)) r
{-# INLINE bmfromStream #-}

bmchunks :: MBundle m v a -> Stream m (Chunk v a)
bmchunks = sChunks
{-# INLINE bmchunks #-}

bmsize :: MBundle m v a -> Size
bmsize = sSize
{-# INLINE bmsize #-}

nrun :: New v a -> ST s (Mutable v s a)
nrun (New p) = p
{-# INLINE nrun #-}

nunstream :: GVector v a => Bundle v a -> New v a
nunstream s = s `seq` New (gmvunstream s)
{-# INLINE [1] nunstream #-}

gmunsafeNew :: (PrimMonad m, GMVector v a) => Int -> m (v (PrimState m) a)
gmunsafeNew n = {- UNSAFE_CHECK(checkLength) "unsafeNew" n
              $ -} gmbasicUnsafeNew n
{-# INLINE gmunsafeNew #-}

gmvunstream :: (PrimMonad m, GVector v a)
            => Bundle v a -> m (Mutable v (PrimState m) a)
gmvunstream s = gmvmunstream (blift s)
{-# INLINE [1] gmvunstream #-}

gmvmunstream :: (PrimMonad m, GVector v a)
             => MBundle m v a -> m (Mutable v (PrimState m) a)
gmvmunstream s = case upperBound (bmsize s) of
               Just n  -> gmvmunstreamMax     s n
               Nothing -> gmvmunstreamUnknown s
{-# INLINE [1] gmvmunstream #-}

gmvmunstreamMax :: (PrimMonad m, GVector v a)
                => MBundle m v a -> Int -> m (Mutable v (PrimState m) a)
gmvmunstreamMax s n
  = do
      v <- {-INTERNAL_CHECK(checkLength) "munstreamMax" n
           $-} gmunsafeNew n
      let {-# INLINE [0] copyChunk #-}
          copyChunk i (Chunk m f) =
            {-INTERNAL_CHECK(checkSlice) "munstreamMax.copyChunk" i m (length v) $-} do
              f (gmbasicUnsafeSlice i m v)
              return (i+m)

      n' <- sfoldlM' copyChunk 0 (bmchunks s)
      return -- $ INTERNAL_CHECK(checkSlice) "munstreamMax" 0 n' n
             $ gmunsafeSlice 0 n' v
{-# INLINE gmvmunstreamMax #-}

gmvmunstreamUnknown :: (PrimMonad m, GVector v a)
                    => MBundle m v a -> m (Mutable v (PrimState m) a)
gmvmunstreamUnknown s
  = do
      v <- gmunsafeNew 0
      (v', n) <- sfoldlM copyChunk (v,0) (bmchunks s)
      return -- $ INTERNAL_CHECK(checkSlice) "munstreamUnknown" 0 n (length v')
             $ gmunsafeSlice 0 n v'
  where
    {-# INLINE [0] copyChunk #-}
    copyChunk (v,i) (Chunk n f)
      = do
          let j = i+n
          v' <- if gmbasicLength v < j
                  then gmunsafeGrow v (delay_inline max (enlarge_delta v) (j - gmbasicLength v))
                  else return v
          {-INTERNAL_CHECK(checkSlice) "munstreamUnknown.copyChunk" i n (length v')
            $-}
          f (gmbasicUnsafeSlice i n v')
          return (v',j)
{-# INLINE gmvmunstreamUnknown #-}

gmunsafeSlice :: GMVector v a => Int
                              -> Int
                              -> v s a
                              -> v s a
gmunsafeSlice i n v = {- UNSAFE_CHECK(checkSlice) "unsafeSlice" i n (length v)
                    $ -} gmbasicUnsafeSlice i n v
{-# INLINE gmunsafeSlice #-}

gmlength :: GMVector v a => v s a -> Int
gmlength = gmbasicLength
{-# INLINE gmlength #-}

gmunsafeGrow :: (PrimMonad m, GMVector v a)
             => v (PrimState m) a -> Int -> m (v (PrimState m) a)
gmunsafeGrow v n = {- UNSAFE_CHECK(checkLength) "unsafeGrow" n
                 $ -} gmbasicUnsafeGrow v n
{-# INLINE gmunsafeGrow #-}

enlarge_delta :: GMVector v a => v s a -> Int
enlarge_delta v = max (gmlength v) 1

gnew :: GVector v a => New v a -> v a
gnew m = m `seq` runST (gunsafeFreeze =<< nrun m)
{-# INLINE [1] gnew #-}

gstream :: GVector v a => v a -> Bundle v a
gstream v = gstream' v
{-# INLINE [1] gstream #-}

gstream' :: GVector v a => v a -> Bundle v a
gstream' v = bfromVector v
{-# INLINE gstream' #-}

gunsafeFreeze :: (PrimMonad m, GVector v a) => Mutable v (PrimState m) a -> m (v a)
gunsafeFreeze = gbasicUnsafeFreeze
{-# INLINE gunsafeFreeze #-}

gunstream :: GVector v a => Bundle v a -> v a
gunstream s = gnew (nunstream s)
{-# INLINE gunstream #-}

