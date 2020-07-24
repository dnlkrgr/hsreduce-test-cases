{-# LANGUAGE FlexibleContexts, GADTs, MultiParamTypeClasses, RankNTypes, TypeFamilies #-}
module Bug (
        gcons
    ) where
import Data.Functor.Identity
import GHC.ST
import GHC.Types
gcons :: GVector v a => a -> v a -> v a
gcons x v = gunstream $ bcons x $ gstream v
{-# INLINE gcons #-}
data New v a = New (forall s. ST s (Mutable v s a))
data Chunk v a
  = Chunk Int (forall m.
               (PrimMonad m, GVector v a) => Mutable v (PrimState m) a -> m ())
data Step s a
  where
    Yield :: a -> s -> Step s a
    Skip :: s -> Step s a
    Done :: Step () ()
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
class Monad m => PrimMonad m where
  type PrimState m
instance PrimMonad (ST s) where
  type PrimState (ST s) = s
type family Mutable (v :: Type -> Type) :: Type -> Type -> Type
class GMVector v a where
  gmbasicLength :: v s a -> Int
  gmbasicUnsafeSlice :: () -> Int -> v s a -> v s a
  gmbasicUnsafeNew :: () -> m (v (PrimState m) a)
  gmbasicUnsafeWrite :: v (PrimState m) a -> () -> a -> m ()
  gmbasicUnsafeGrow :: () -> () -> m (v () a)
class GMVector (Mutable v) a => GVector v a where
  gbasicUnsafeFreeze :: Mutable v (PrimState m) a -> m (v a)
  gbasicLength :: v a -> ()
  gbasicUnsafeIndexM :: v () -> () -> m a
  gbasicUnsafeCopy :: () -> v a -> m ()
  gelemseq :: v a -> ()
sfoldlM m w (Stream step t)
  = foldlM_loop undefined w t
  where
      foldlM_loop _ z s
        = do r <- step s
             case r of
               Yield x s'
                 -> do z' <- m z x
                       foldlM_loop undefined z' s'
               _ -> return undefined
{-# INLINE [1] sfoldlM #-}
Stream stepa _ `sappend` Stream stepb _
  = Stream step (Left undefined)
  where
      step (Left _)
        = do r <- stepa undefined
             case r of { Yield x _ -> return $ Yield x undefined }
      step _
        = do r <- stepb undefined
             case r of
               Yield x _ -> return $ Yield x undefined
               Skip sb' -> return $ Skip (Right sb')
{-# INLINE [1] sappend #-}
ssingleton x
  = Stream (return . step) undefined
  where
      step _ = Yield x undefined
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
  = Bundle undefined (Stream vstep undefined) undefined undefined
  where
      vstep True
        = return
            (Yield
               (Chunk undefined (\ _ -> gbasicUnsafeCopy undefined v)) undefined)
bcons = bmcons
bmcons x s = bmappend (bmsingleton x) s
bmsingleton x = bmfromStream (ssingleton x) undefined
{-# INLINE [1] bmsingleton #-}
bmfromStream (Stream step _) _
  = Bundle undefined (Stream step' undefined) undefined undefined
  where
      step' _
        = do r <- step undefined
             return
               $ fmap (\ x -> Chunk 1 (\ v -> gmbasicUnsafeWrite v undefined x)) r
bmchunks = sChunks
nrun (New p) = p
nunstream :: GVector v a => Bundle v a -> New v a
nunstream s = New (gmvunstream s)
gmunsafeNew _ = gmbasicUnsafeNew undefined
gmvunstream s = gmvmunstream (blift s)
{-# INLINE [1] gmvunstream #-}
gmvmunstream s = gmvmunstreamUnknown s
gmvmunstreamUnknown s
  = do v <- gmunsafeNew undefined
       _ <- sfoldlM copyChunk (v, undefined) (bmchunks s)
       return $ undefined
  where
      copyChunk (v, _) (Chunk n f)
        = do let j = n
             v' <- if gmbasicLength v < j then undefined else return undefined
             f (gmbasicUnsafeSlice undefined n v')
             return (v', j)
{-# INLINE gmvmunstreamUnknown #-}
gnew m = runST (gunsafeFreeze =<< nrun m)
gstream v = gstream' v
gstream' v = bfromVector v
gunsafeFreeze ::
  GVector v a => Mutable v (PrimState m) a -> m (v a)
gunsafeFreeze = gbasicUnsafeFreeze
gunstream s = gnew (nunstream s)
