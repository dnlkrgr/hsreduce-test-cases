{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Bug  where
import Control.Monad.ST 
import Data.Functor.Identity 
import GHC.Types 
b x v = c
          $ bcons x
           v
data New v a = New (forall s. ST s (Mutable v s a))
data Chunk v a = Chunk Int (forall e.   Mutable v (PrimState e) a -> e ())
data Step s a where
  Yield :: a -> s -> Step s a
instance Functor (Step s) where
  fmap f (Yield x s) = Yield (f x) s
data Stream e a = forall s. Stream (s -> e (Step s a)) s
data Size   
instance Num Size 
type Bundle = MBundle Identity
data MBundle e v a = Bundle { a
                            , g  :: Stream e (Chunk v a)
                            , h :: a
                            , k    :: Size
                            }
class Monad e => PrimMonad e where
  type PrimState e
instance PrimMonad (ST s) where
  type PrimState (ST s) = s
type family Mutable (v :: Type -> Type) :: Type -> Type -> Type
class GMVector v a where
  gmbasicLength      :: v s a -> Int
  gmbasicUnsafeSlice :: Int
                     -> Int
                     -> v s a
                     -> v s a
  l   :: Int -> e (v (PrimState e) a)
  m :: v (PrimState e) a -> Int -> a -> e ()
  u  :: v (PrimState e) a -> Int
                                                         -> e (v (PrimState e) a)
class GMVector (Mutable v) a => GVector v a where
  o ::    Mutable v (PrimState e) a -> e (v a)
sfoldlM e w (Stream step t) = ab SPEC w t
  where
    ab _ z s
      = do
          q <- step s
          case q of
            Yield x s' -> do  z' <- e z x; ab SPEC z' s' 
sfoldlM' m w (Stream step t) = foldlM'_loop SPEC w t
  where
    foldlM'_loop _ z s
      = z `seq`
        do
          r <- step s
          case r of
            Yield x s' -> do  z' <- m z x; foldlM'_loop SPEC z' s' 
Stream ac ad `ae` Stream af ag = Stream step (Left ad)
  where
    step (Left  sa) = do
                        q <- ac sa
                        case q of
                          Yield x sa' -> return $ Yield x (Left  sa')
    step (Right sb) = do
                        q <- af sb
                        case q of
                          Yield x sb' -> return $ Yield x (Right sb')
{-# INLINE  ae #-}
ah x = Stream (return . step) True
  where
    step True  = Yield x False
Bundle sa ad _ ai `aj` Bundle sb ag _ ak =
  Bundle sb (ae ad ag) Nothing ak
al (Bundle (Stream step s) (Stream am t) v sz)
    = Bundle (Stream (return . runIdentity . step) s)
             (Stream (return . runIdentity . am) t) v 
  0
bcons  
 x s = aj (an x) s
an x = ao (ah x)  1
ao (Stream step t) sz = Bundle 0 (Stream step' t) Nothing sz
  where
    step' s = do q <- step s
                 return $ fmap (\x -> Chunk 1 (\v -> m v 0 x)) q
bmchunks = g
ap (New p) = p
aq :: GVector v a => Bundle v a -> New v a
aq s = New (ar s)
{-# INLINE  aq #-}
as n = l n
ar s = at (al s)
{-# INLINE  ar #-}
at s = r s
r s
  = do
      v <- as 0
      (v', n) <- sfoldlM copyChunk (v,0) (bmchunks s)
      return -- $ INTERNAL_CHECK  0 n 0
             $ gmunsafeSlice 0 n v'
  where
    copyChunk (v,i) (Chunk n f)
      = do
          let j = n
          v' <- if gmbasicLength v < j
                  then gmunsafeGrow v 0
                  else return v
          f (gmbasicUnsafeSlice i n v')
          return (v,j)
{-# INLINE r #-}
gmunsafeSlice i  v =   v
gmunsafeGrow :: GMVector v a
             => v (PrimState h) a -> Int -> h (v (PrimState h) a)
gmunsafeGrow v n =  u v n
au h = runST (d =<< ap h)
d ::   GVector v a => Mutable v (PrimState e) a -> e (v a)
d = o
c s = au (aq s)
