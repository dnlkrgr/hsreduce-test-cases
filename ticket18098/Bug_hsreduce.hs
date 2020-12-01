{-# LANGUAGE BangPatterns, FlexibleContexts, GADTs, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, TypeFamilies #-}
module Bug (
        gcons
    ) where
import Data.Functor.Identity
import GHC.ST
import GHC.Types
gcons :: GVector v a => a -> v a -> ()
gcons x v = gunstream $ bcons x $ gstream v
{-# INLINE gcons #-}
data New v a = New (forall s. ST s ())
data Chunk v a
  = Chunk (forall m. GVector v a => Mutable v () a -> m ())
data Step s a where Yield :: a -> s -> Step s a
instance Functor (Step s) where
  {-# INLINE fmap #-}
  fmap f (Yield x _) = Yield (f x) undefined
data Stream m a = forall s. Stream (s -> m (Step s a)) s
type Bundle = MBundle Identity
data MBundle m v a
  = Bundle {_sElems :: (),
            sChunks :: Stream m (Chunk v a),
            _sVector :: (),
            sSize :: ()}
type family Mutable (v :: Type -> Type) :: Type -> Type -> Type
class GMVector v a where
  gmbasicLength :: v () a -> Int
  gmbasicUnsafeSlice :: () -> v () a -> v () a
  gmbasicUnsafeNew :: m (v () a)
  gmbasicUnsafeWrite :: v () a -> a -> m ()
  gmbasicUnsafeGrow :: v () a -> () -> m (v () a)
class GMVector (Mutable v) a => GVector v a where
  gbasicUnsafeCopy :: () -> v a -> m ()
sfoldlM m w (Stream step t)
  = foldlM_loop SPEC w t
  where
      foldlM_loop !_ z s
        = do r <- step s
             case r of {
               Yield x s'
                 -> do z' <- m z x
                       foldlM_loop SPEC z' s' }
{-# INLINE [1] sfoldlM #-}
Stream stepa _ `sappend` Stream stepb _
  = Stream step (Left undefined)
  where
      step (Left _)
        = do r <- stepa undefined
             case r of { Yield x _ -> return $ Yield x undefined }
      step _
        = do r <- stepb undefined
             case r of { Yield x sb' -> return $ Yield x (Right sb') }
{-# INLINE [1] sappend #-}
ssingleton x
  = Stream (return . step) undefined
  where
      step = Yield x
Bundle _ ta _ _ `bmappend` Bundle _ tb _ _
  = Bundle undefined (sappend ta tb) undefined undefined
bfromVector :: GVector v a => v a -> Bundle v a
bfromVector = bmfromVector
blift (Bundle _ (Stream vstep t) _ _)
  = Bundle
      undefined
      (Stream (return . runIdentity . vstep) t)
      undefined
      undefined
bmfromVector v
  = n `seq`
        Bundle undefined (Stream vstep undefined) undefined undefined
  where
      n = v
      vstep True
        = return
            (Yield (Chunk (\ _ -> gbasicUnsafeCopy undefined v)) undefined)
bcons = bmcons
bmcons x = bmappend (bmsingleton x)
bmsingleton x = bmfromStream (ssingleton x) undefined
{-# INLINE [1] bmsingleton #-}
bmfromStream (Stream step _)
  = Bundle undefined (Stream step' undefined) undefined
  where
      step' _
        = do r <- step undefined
             return $ fmap (\ x -> Chunk (\ v -> gmbasicUnsafeWrite v x)) r
bmchunks = sChunks
nrun (New p) = p
nunstream s = New (gmvunstream s)
gmunsafeNew _ = gmbasicUnsafeNew
gmvunstream s = gmvmunstream (blift s)
{-# INLINE [1] gmvunstream #-}
gmvmunstream s = gmvmunstreamUnknown s
gmvmunstreamUnknown s
  = do v <- gmunsafeNew undefined
       _ <- sfoldlM copyChunk (v, 0) (bmchunks s)
       undefined
  where
      copyChunk (v, i) (Chunk f)
        = do let j = i
             v' <- if gmbasicLength v < j then
                       gmunsafeGrow v undefined
                   else
                       undefined
             f (gmbasicUnsafeSlice undefined v')
             return (undefined, j)
{-# INLINE gmvmunstreamUnknown #-}
gmunsafeGrow v = gmbasicUnsafeGrow v
gnew m = runST (nrun m)
gstream v = gstream' v
gstream' v = bfromVector v
gunstream s = gnew (nunstream s)
