{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleInstances, FunctionalDependencies, PolyKinds, RankNTypes, ScopedTypeVariables, TypeApplications, UndecidableInstances #-}
module Bug (
        strs
    ) where
import Control.Applicative
strs = \ _ _ _ -> foldMapOf param
foldMapOf :: Getting r s () -> s -> r
foldMapOf l = l undefined
type Getting r s a = (() -> Const () ()) -> s -> r
class HasParam p s t a b where
  param :: (a -> g b) -> s -> g t
instance HasParam n s t a () where
  param
    = \ f
        -> lowerYoneda . lowerCurried
             . \ _ -> gparam (liftCurriedYoneda . f) undefined
  {-# INLINE param #-}
newtype Yoneda f a = Yoneda {runYoneda :: forall b. () -> f b}
newtype Curried f a = Curried {runCurried :: forall r. () -> f r}
instance Functor (Curried f) where
  fmap = undefined
  {-# INLINE fmap #-}
instance Applicative (Curried f) where
  pure = undefined
  {-# INLINE pure #-}
  _ <*> _ = undefined
  {-# INLINE (<*>) #-}
lowerYoneda (Yoneda f) = f undefined
lowerCurried (Curried f) = f undefined
liftCurriedYoneda _ = Curried undefined
class GHasParam where
  gparam :: Applicative g => (a -> g ()) -> () -> g ()
instance {-# OVERLAPPABLE #-} GHasParam where
  gparam f = gparamRec @(('Just ())) f
class GHasParamRec param s t a b where
  gparamRec :: (a -> g b) -> s -> g t
instance (HasParam n s t a b) =>
         GHasParamRec ('Just ()) s t a b where
  gparamRec = param @n
