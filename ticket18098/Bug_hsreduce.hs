{-# LANGUAGE FlexibleContexts, GADTs, MultiParamTypeClasses, RankNTypes, TypeFamilies #-}
module Bug (
        gcons
    ) where
import Data.Functor.Identity
import GHC.ST
import GHC.Types
gcons x v
  = gunstream
      $ (\ (Bundle _ ta _) (Bundle _ tb _)
           -> Bundle undefined (sappend ta tb) undefined)
          (bmfromStream (ssingleton x) undefined)
      $ v
{-# INLINE gcons #-}
data New v a = New (forall s. ST s (Mutable v () ()))
data Chunk v a = Chunk (forall m. Mutable v () a -> m ())
data Step s a where Yield :: a -> s -> Step s a
data Stream m a = forall s. Stream (s -> m (Step s a)) s
data MBundle m v a
  = Bundle {_sElems :: (),
            sChunks :: Stream m (Chunk v a),
            sSize :: ()}
class Monad m => PrimMonad m where
  type PrimState m
instance PrimMonad (ST s) where
  type PrimState _ = ()
type family Mutable (v :: Type -> Type) :: Type -> Type -> Type
class GMVector v a where
  gmbasicLength :: v () a -> Int
  gmbasicUnsafeNew :: m (v () a)
  gmbasicUnsafeWrite :: v () () -> a -> m ()
  gmbasicUnsafeGrow :: v () a -> Int -> m (v () a)
class GMVector (Mutable v) a => GVector v a
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
      step (Left sa)
        = do r <- stepa sa
             case r of { Yield x _ -> return $ Yield x undefined }
      step _
        = do r <- stepb undefined
             case r of { Yield x _ -> return $ Yield x undefined }
ssingleton x
  = Stream (return . step) undefined
  where
      step = Yield x
blift (Bundle _ (Stream vstep t) _)
  = Bundle
      undefined (Stream (return . runIdentity . vstep) t) undefined
{-# INLINE [1] blift #-}
bmfromStream (Stream step _)
  = Bundle undefined (Stream step' undefined)
  where
      step' _
        = do r <- step undefined
             return
               $ (\ (Yield x _)
                    -> Yield (Chunk (\ v -> gmbasicUnsafeWrite v x)) undefined)
                   r
gmvmunstream ::
  (PrimMonad m, GVector v a) => MBundle m v a -> m (Mutable v () ())
gmvmunstream = gmvmunstreamUnknown
{-# INLINE [1] gmvmunstream #-}
gmvmunstreamUnknown s
  = do v <- gmbasicUnsafeNew
       _ <- sfoldlM copyChunk (v, undefined) (sChunks s)
       undefined
  where
      copyChunk (v, i) (Chunk f)
        = do let j = i
             v' <- gmbasicUnsafeGrow v (max (max (gmbasicLength v) 1) j)
             f v'
             return (undefined, undefined)
{-# INLINE gmvmunstreamUnknown #-}
gnew :: New v () -> v ()
gnew m = runST (undefined =<< (\ (New p) -> p) m)
gunstream s = gnew (New (gmvmunstream (blift s)))
