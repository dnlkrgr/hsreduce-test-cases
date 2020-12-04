{-# LANGUAGE FlexibleContexts, GADTs, MultiParamTypeClasses, RankNTypes, TypeFamilies #-}
module Bug (
        gcons
    ) where
import Data.Functor.Identity
import GHC.ST
import GHC.Types
gcons :: GVector v a => a -> v a -> ()
gcons x v = gunstream $ bcons x $ bfromVector v
{-# INLINE gcons #-}
data New v a = New (forall s. ST s ())
data Chunk v a
  = Chunk Int (forall m.
               (PrimMonad m, GVector v a) => Mutable v (PrimState m) a -> m ())
data Step s a where Yield :: a -> s -> Step s a
data Stream m a = forall s. Stream (s -> m (Step s a)) s
type Bundle = MBundle Identity
data MBundle m v a = Bundle {sChunks :: Stream m (Chunk v a)}
class Monad m => PrimMonad m where
  type PrimState m
instance PrimMonad (ST s) where
  type PrimState _ = ()
type family Mutable (v :: Type -> Type) :: Type -> Type -> Type
class GMVector v a where
  gmbasicLength :: v () a -> Int
  gmbasicUnsafeSlice :: () -> () -> v () a -> v () a
  gmbasicUnsafeNew :: m (v () a)
  gmbasicUnsafeWrite :: v (PrimState m) a -> a -> m ()
  gmbasicUnsafeGrow :: v () a -> () -> m (v () a)
class GMVector (Mutable v) a => GVector v a where
  gbasicUnsafeCopy :: v a -> m ()
sfoldlM m w (Stream step t)
  = foldlM_loop w t
  where
      foldlM_loop z s
        = do r <- step s
             case r of {
               Yield x s'
                 -> do z' <- m z x
                       foldlM_loop z' s' }
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
bfromVector :: GVector v a => v a -> Bundle v a
bfromVector = bmfromVector
blift (Bundle (Stream vstep t))
  = Bundle (Stream (return . runIdentity . vstep) t)
{-# INLINE [1] blift #-}
bmfromVector v
  = Bundle (Stream vstep undefined)
  where
      vstep _
        = return
            (Yield (Chunk undefined (\ _ -> gbasicUnsafeCopy v)) undefined)
bcons = bmcons
bmcons x
  = (\ (Bundle ta) (Bundle tb) -> Bundle (sappend ta tb))
      (bmsingleton x)
bmsingleton x = bmfromStream (ssingleton x)
{-# INLINE [1] bmsingleton #-}
bmfromStream (Stream step _)
  = Bundle (Stream step' undefined)
  where
      step' _
        = do r <- step undefined
             return
               $ (\ (Yield x _)
                    -> Yield (Chunk 1 (\ v -> gmbasicUnsafeWrite v x)) undefined)
                   r
gmvmunstream s = gmvmunstreamUnknown s
{-# INLINE [1] gmvmunstream #-}
gmvmunstreamUnknown s
  = do v <- gmbasicUnsafeNew
       _ <- sfoldlM copyChunk (v, undefined) (sChunks s)
       undefined
  where
      copyChunk (v, _) (Chunk n f)
        = do let j = n
             v' <- if gmbasicLength v < j then
                       gmbasicUnsafeGrow v undefined
                   else
                       undefined
             f (gmbasicUnsafeSlice undefined undefined v')
             return (undefined, undefined)
{-# INLINE gmvmunstreamUnknown #-}
gunstream s
  = runST ((\ (New p) -> p) (New (gmvmunstream (blift s))))
