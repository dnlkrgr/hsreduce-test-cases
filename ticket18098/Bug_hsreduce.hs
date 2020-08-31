{-# LANGUAGE BangPatterns, FlexibleContexts, GADTs, MultiParamTypeClasses, RankNTypes, TypeFamilies #-}
module Bug (
        a
    ) where
import Data.Functor.Identity
import GHC.ST
import GHC.Types
a :: C b a => a -> b a -> ()
a d b = e $ bcons d $ f b
{-# INLINE a #-}
data G b a = G (forall h. ST h ())
data Chunk b a
  = Chunk Int (forall i. C b a => J b (PrimState i) a -> i ())
data K h a
  where
    L :: a -> h -> K h a
    M :: () -> K () ()
    Done :: K () ()
instance Functor (K h) where
  {-# INLINE fmap #-}
  fmap f (L d _) = L (f d) undefined
data N i a = forall h. N (h -> i (K h a)) h
type Bundle = MBundle Identity
data MBundle i b a
  = Bundle {r :: N i (Chunk b a), _sVector :: (), s :: ()}
class Monad i => PrimMonad i where
  type PrimState i
instance PrimMonad (ST h) where
  type PrimState (ST h) = ()
type family J (b :: Type -> Type) :: Type -> Type -> Type
class GMVector b a where
  gmbasicLength :: b () a -> Int
  gmbasicUnsafeSlice :: () -> () -> b () a -> b () a
  gmbasicUnsafeNew :: () -> i (b () a)
  gmbasicUnsafeWrite :: b (PrimState i) a -> () -> a -> i ()
  gmbasicUnsafeGrow :: () -> () -> i (b () a)
class GMVector (J b) a => C b a where
  gbasicUnsafeFreeze :: () -> i (b a)
  gbasicLength :: b a -> ()
  gbasicUnsafeIndexM :: b () -> () -> i a
  gbasicUnsafeCopy :: () -> b a -> i ()
  gelemseq :: b a -> ()
sfoldlM i w (N step t)
  = foldlM_loop SPEC w t
  where
      foldlM_loop !_ t h
        = do r <- step h
             case r of {
               L d s'
                 -> do u <- i t d
                       foldlM_loop SPEC u s' }
{-# INLINE [1] sfoldlM #-}
N stepa ta `sappend` N stepb _
  = N step (Left ta)
  where
      step (Left _)
        = do r <- stepa undefined
             case r of { L d _ -> return $ L d (Left undefined) }
      step _
        = do r <- stepb undefined
             case r of { L d _ -> return $ L d undefined }
{-# INLINE [1] sappend #-}
ssingleton d
  = N (return . step) True
  where
      step _ = L d undefined
Bundle ta _ _ `bmappend` Bundle tb _ _
  = Bundle (sappend ta tb) undefined undefined
bfromVector :: C b a => b a -> Bundle b a
bfromVector = bmfromVector
blift (Bundle (N v t) _ _)
  = Bundle (N (return . runIdentity . v) t) undefined undefined
bmfromVector b
  = Bundle (N v undefined) undefined undefined
  where
      n = b
      v True
        = return
            (L (Chunk undefined (\ _ -> gbasicUnsafeCopy undefined b))
               undefined)
bcons = bmcons
bmcons d h = bmappend (bmsingleton d) h
bmsingleton d = bmfromStream (ssingleton d) undefined
{-# INLINE [1] bmsingleton #-}
bmfromStream (N step t) _
  = Bundle (N step' t) undefined undefined
  where
      step' _
        = do r <- step undefined
             return
               $ fmap (\ d -> Chunk 1 (\ b -> gmbasicUnsafeWrite b undefined d)) r
bmchunks = r
nrun (G p) = p
nunstream h = G (gmvunstream h)
gmunsafeNew _ = gmbasicUnsafeNew undefined
gmvunstream h = gmvmunstream (blift h)
{-# INLINE [1] gmvunstream #-}
gmvmunstream h = gmvmunstreamUnknown h
{-# INLINE [1] gmvmunstream #-}
gmvmunstreamUnknown h
  = do b <- gmunsafeNew undefined
       _ <- sfoldlM copyChunk (b, 0) (bmchunks h)
       undefined
  where
      copyChunk (b, i) (Chunk n f)
        = do let j = i + n
             v' <- if gmbasicLength b < j then
                       gmunsafeGrow undefined undefined
                   else
                       undefined
             f (gmbasicUnsafeSlice undefined undefined v')
             return (v', j)
{-# INLINE gmvmunstreamUnknown #-}
gmunsafeGrow _ _ = gmbasicUnsafeGrow undefined undefined
gnew i = runST (nrun i)
f b = gstream' b
gstream' b = bfromVector b
e h = gnew (nunstream h)
