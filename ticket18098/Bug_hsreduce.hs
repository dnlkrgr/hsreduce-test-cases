{-# LANGUAGE BangPatterns, FlexibleContexts, GADTs, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, TypeFamilies #-}
module Bug (
        gcons
    ) where
import Control.Monad.ST ( runST )
import Data.Kind ( Type )
import Data.Functor.Identity ( Identity(..) )
import GHC.ST ( ST(..) )
import GHC.Types ( SPEC(..) )
gcons :: forall v a. GVector v a => a -> v a -> ()
gcons x v
  = gelemseq (undefined :: v a) undefined $ gunstream $ bcons x
      $ gstream v
{-# INLINE gcons #-}
data New v a = New (forall s. ST s ())
data Chunk v a
  = Chunk Int (forall m.
               GVector v a => Mutable v (PrimState m) a -> m ())
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
  type PrimState (ST s) = ()
type family Mutable (v :: Type -> Type) :: Type -> Type -> Type
class GMVector v a where
  gmbasicLength :: v () a -> Int
  gmbasicUnsafeSlice :: () -> () -> v () a -> v () a
  gmbasicUnsafeNew :: () -> m (v () a)
  gmbasicUnsafeWrite :: v (PrimState m) a -> () -> a -> m ()
  gmbasicUnsafeGrow :: () -> () -> m (v () a)
class GMVector (Mutable v) a => GVector v a where
  gbasicUnsafeFreeze :: () -> m (v a)
  gbasicLength :: v a -> ()
  gbasicUnsafeIndexM :: v () -> () -> m a
  gbasicUnsafeCopy :: () -> v a -> m ()
  gelemseq :: v a -> () -> () -> ()
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
  = v `seq` n
      `seq` Bundle undefined (Stream vstep undefined) undefined undefined
  where
      n = gbasicLength v
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
nunstream s = s `seq` New (gmvunstream s)
gmunsafeNew _ = gmbasicUnsafeNew undefined
gmvunstream s = gmvmunstream (blift s)
{-# INLINE [1] gmvunstream #-}
gmvmunstream s = gmvmunstreamUnknown s
{-# INLINE [1] gmvmunstream #-}
gmvmunstreamUnknown s
  = do v <- gmunsafeNew undefined
       _ <- sfoldlM copyChunk (v, 0) (bmchunks s)
       undefined
  where
      copyChunk (v, i) (Chunk n f)
        = do let j = i + n
             v' <- if gmbasicLength v < j then
                       gmunsafeGrow undefined undefined
                   else
                       undefined
             f (gmbasicUnsafeSlice undefined undefined v')
             return (v', j)
{-# INLINE gmvmunstreamUnknown #-}
gmunsafeGrow _ _ = gmbasicUnsafeGrow undefined undefined
gnew m = m `seq` runST (undefined =<< nrun m)
gstream v = gstream' v
gstream' v = bfromVector v
gunstream s = gnew (nunstream s)
