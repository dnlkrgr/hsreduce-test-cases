{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Bug (c) where
import Data.Functor.Identity 
import GHC.ST 
import GHC.Types 
c x v = d
          $ bcons x
          $ gstream v
data New v a = New (forall s. ST s (Mutable v s a))
data Chunk v a = Chunk Int (forall e. Mutable v (PrimState e) a -> e ())
data Step s a where
  Yield :: a -> s -> Step s a
  Done  :: Step s a
data Stream e a = forall s. Stream (s -> e (Step s a)) s
data Size  
instance Num Size 
type Bundle = MBundle Identity
data MBundle e v a = Bundle { a
                            , g  :: Stream e (Chunk v a)
                            , h :: Maybe (v a)
                            , f    :: Size
                            }
class Monad e => PrimMonad e where
  type PrimState e
instance PrimMonad (ST s) where
  type PrimState (ST s) = s
type family Mutable (v :: Type -> Type) :: Type -> Type -> Type
class GMVector v a where
  l      :: v s a -> Int
  gmbasicUnsafeSlice :: Int
                     -> Int
                     -> v s a
                     -> v s a
  k   :: Int -> e (v (PrimState e) a)
  p  :: v (PrimState e) a -> Int
                                                         -> e (v (PrimState e) a)
class GMVector (Mutable v) a => GVector v a where
  m :: Mutable v (PrimState e) a -> e (v a)
  o   :: Mutable v (PrimState e) a -> v a -> e b
sfoldlM e w (Stream step t) = q SPEC w t
  where
    q _ z s
      = do
          r <- step s
          case r of
            Yield x s' -> do  z' <- e z x; q SPEC z' s' 
sfoldlM' m w (Stream step t) = foldlM'_loop SPEC w t
  where
    foldlM'_loop _ z s
      = z `seq`
        do
          r <- step s
          case r of
            Yield x s' -> do  z' <- m z x; foldlM'_loop SPEC z' s' 
Stream aa ab `ac` Stream ad ae = Stream step (Left ab)
  where
    step (Left  sa) = do
                        r <- aa sa
                        case r of
                          Yield x sa' -> return $ Yield x (Left  sa')
    step (Right sb) = do
                        r <- ad sb
                        case r of
                          Yield x sb' -> return $ Yield x (Right sb')
Bundle sa ab _ ag `ah` Bundle sb ae _ ai =
  Bundle sb (ac ab ae) Nothing   ai
u :: GVector v a => v a -> Bundle v a
u = aj
ak (Bundle (Stream step s) (Stream al t) v sz)
    = Bundle (Stream (return . runIdentity . step) s)
             (Stream (return . runIdentity . al) t) v sz
aj v = Bundle (Stream step 0)
                                        (Stream al True)
                                        (Just v)
                                        0
  where
    step n = return Done
    al True  = return (Yield (Chunk 0 (\am -> o am v)) False)
bcons x s = ah 0 s
an    step     
    step' s = do r <- step s
                 return  r
bmchunks = g
nrun (New p) = p
ao :: GVector v a => Bundle v a -> New v a
ao s = New (ap s)
y n =  k n
ap s = aq (ak s)
aq s
  = do
      v <- y 0
      v'  <- sfoldlM copyChunk (v,0) (bmchunks s)
      v'
  where
    copyChunk (v,i) (Chunk n f)
      = do
          let j = n
          v' <- if l v < j
                  then gmunsafeGrow v 0
                  else return v
          f (gmbasicUnsafeSlice i n v')
          return (v',j)
gmunsafeGrow v n =  p v n
gnew m = runST (gunsafeFreeze =<< nrun m)
gstream v = u v
gunsafeFreeze ::   GVector v a => Mutable v (PrimState e) a -> e (v a)
gunsafeFreeze = m
d s = gnew (ao s)
