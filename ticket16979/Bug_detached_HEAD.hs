{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DeriveGeneric, FlexibleInstances, FunctionalDependencies, InstanceSigs, PolyKinds, RankNTypes, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Bug (
        strs
    ) where
import Control.Applicative
import Data.Coerce
import Data.Kind
import Data.Monoid
import GHC.Generics
import GHC.TypeLits
data Poly a b
  = PCons a (Poly b a)
  deriving Generic
poly = ((PCons 20 undefined))
strs = toListOf (param @0) poly
toListOf l = foldrOf l undefined undefined
foldrOf l _ _ = flip appEndo undefined . foldMapOf l undefined
foldMapOf l _ = getConst #. l undefined
class Profunctor p where
  dimap :: () -> () -> () -> ()
  (#.) :: Coercible c b => q b c -> p a b -> p a c
instance Profunctor (->) where
  dimap _ _ _ = undefined
  {-# INLINE dimap #-}
  (#.) _ = coerce :: Coercible b a => a -> b
  {-# INLINE (#.) #-}
class HasParam p s t a b where
  param :: Applicative g => (a -> g b) -> s -> g t
instance (GenericN t, s ~ t, GHasParam n (RepN s) (RepN t) a b) =>
         HasParam n s t a b where
  param = confusing (\ f s -> toN <$> gparam @n f (fromN s))
  {-# INLINE param #-}
confusing t
  = \ f -> lowerYoneda . lowerCurried . t (liftCurriedYoneda . f)
newtype Yoneda f a
  = Yoneda {runYoneda :: forall b. (a -> b) -> f b}
instance Functor (Yoneda f) where
instance Applicative (Yoneda f) where
newtype Curried f a
  = Curried {runCurried :: forall r. f (a -> r) -> f r}
instance Functor (Curried f) where
  fmap _ (Curried g) = Curried (g . undefined)
  {-# INLINE fmap #-}
instance Applicative (Curried f) where
  pure _ = undefined
  {-# INLINE pure #-}
  _ <*> Curried ma = Curried (ma . undefined . undefined)
  {-# INLINE (<*>) #-}
lowerYoneda (Yoneda f) = f id
lowerCurried (Curried f) = f (pure id)
liftCurriedYoneda fa = Curried (`yap` fa)
yap (Yoneda k) fa = undefined (\ _ -> k (undefined .) <*> fa)
class GHasParam p s t a b where
  gparam :: Applicative g => (a -> g b) -> s () -> g (t ())
instance GHasParam p r r' a b =>
         GHasParam p (l :*: r) (l' :*: r') a b where
  gparam f (_ :*: r) = (:*:) <$> undefined <*> gparam @p f r
instance GHasParam p s t a b =>
         GHasParam p (M1 () meta s) (M1 () meta t) a b where
  gparam f (M1 x) = M1 <$> gparam @p f x
instance {-# OVERLAPPABLE #-} (GHasParamRec (LookupParam si p) s t a b) =>
                              GHasParam p (Rec si s) (Rec ti t) a b where
  gparam f (Rec (K1 x))
    = Rec . K1 <$> gparamRec @(LookupParam si p) f x
class GHasParamRec param s t a b where
  gparamRec :: Applicative g => (a -> g b) -> s -> g t
instance (HasParam n s t a b) =>
         GHasParamRec ('Just n) s t a b where
  gparamRec = param @n
type family LookupParam a p where
  LookupParam (a (_ (m))) n = ((('Just 0)))
type family Param where
type family Indexed (t :: k) (i :: Nat) :: k where
  Indexed (t a) i = Indexed t (1) (Param i)
  Indexed t _ = t
newtype Rec p a x = Rec {unRec :: K1 () a x}
type family Zip a b where
  Zip (M1 mt m s) (M1 mt m t) = M1 () m (M1 () m (Zip s t))
  Zip (l :*: r) (l' :*: r') = Zip l l' :*: Zip r r'
  Zip (Rec0 p) (Rec0 a) = Rec p a
class GenericN a where
  type RepN a :: Type -> Type
  type RepN a = Zip (Rep (Indexed a 0)) (Rep a)
  toN :: RepN a () -> a
  fromN :: a -> RepN a ()
instance (Coercible (Rep a) (RepN a), Generic a) =>
         GenericN a where
  toN :: RepN a () -> a
  toN = undefined
  {-# INLINE toN #-}
  fromN :: forall x. a -> RepN a x
  fromN = coerce (from :: a -> Rep a x)
  {-# INLINE fromN #-}
