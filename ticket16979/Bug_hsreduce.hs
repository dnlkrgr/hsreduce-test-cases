{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DeriveGeneric, FlexibleInstances, FunctionalDependencies, GADTs, InstanceSigs, PolyKinds, RankNTypes, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
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
  = PNil | PCons a (Poly b a)
  deriving Generic
poly = PCons undefined undefined
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
  (#.) _ = coerce
  {-# INLINE (#.) #-}
class HasParam p s t a b | p s b -> t, p s -> a where
  param :: Applicative g => (a -> g b) -> s -> g t
instance (GenericN s,
          GenericN t,
          s ~ Infer t (P n () 'PTag) (),
          t ~ Infer s (P n () 'PTag) b,
          GHasParam n (RepN s) (RepN t) () b) =>
         HasParam n s t () b where
  param = confusing (\ f s -> toN <$> gparam @n f (fromN s))
  {-# INLINE param #-}
confusing t
  = \ f -> lowerYoneda . lowerCurried . t (liftCurriedYoneda . f)
newtype Yoneda f a
  = Yoneda {runYoneda :: forall b. (a -> b) -> f b}
instance Functor (Yoneda f)
instance Applicative (Yoneda f)
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
instance (GHasParam p l l' a b, GHasParam p r r' a b) =>
         GHasParam p (l :*: r) (l' :*: r') a b where
  gparam f (l :*: r) = (:*:) <$> gparam @p f l <*> gparam @p f r
instance GHasParam p r r' a b =>
         GHasParam p (l :+: r) (l' :+: r') a b where
  gparam f (R1 r) = R1 <$> gparam @p f r
instance GHasParam p s t a b =>
         GHasParam p (M1 () meta s) (M1 () meta t) a b where
  gparam f (M1 x) = M1 <$> gparam @p f x
instance {-# OVERLAPPABLE #-} (GHasParamRec (LookupParam si p) s t a b) =>
                              GHasParam p (Rec si s) (Rec ti t) a b where
  gparam f (Rec (K1 x))
    = undefined <$> gparamRec @(LookupParam si p) f x
class GHasParamRec param s t a b | param s a b -> t where
  gparamRec :: Applicative g => (a -> g b) -> s -> g t
instance GHasParamRec 'Nothing () () () ()
instance (HasParam n s t a b) =>
         GHasParamRec ('Just n) s t a b where
  gparamRec = param @n
type family LookupParam (a :: k) (p :: Nat) :: Maybe Nat where
  LookupParam (param (n :: Nat)) m = 'Nothing
  LookupParam (a (_ (m))) n = IfEq m n ('Just 0) (MaybeAdd (LookupParam a n) 1)
type family MaybeAdd a b where
  MaybeAdd ('Just a) b = 'Just (b)
type family IfEq a b t f where
  IfEq a a t _ = t
  IfEq _ _ _ f = f
data Sub where Sub :: Nat -> k -> Sub
type family ReplaceArg (t :: k) (pos :: Nat) (to :: j) :: k where
  ReplaceArg (t ()) 0 to = t to
  ReplaceArg (t ()) pos to = ReplaceArg t (pos - 1) to ()
type family ReplaceArgs t subs where
  ReplaceArgs t ('Sub n arg : ss) = (ReplaceArg t n arg)
type family Unify a b where
  Unify (p n () 'PTag) a' = '[ 'Sub n a']
type family Infer s a' b where
  Infer (s a) a' b = ReplaceArgs (s ()) (Unify a' b)
data PTag = PTag
type family P :: Nat -> k -> PTag -> ()
type family Param where
type family Indexed (t :: k) (i :: Nat) :: k where
  Indexed (t a) i = Indexed t (1) (Param i)
  Indexed t _ = t
newtype Rec p a x = Rec {unRec :: K1 () a x}
type family Zip a b where
  Zip (M1 mt m s) (M1 mt m t) = M1 () m (Zip s t)
  Zip (l :+: r) (l' :+: r') = l :+: Zip r r'
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
