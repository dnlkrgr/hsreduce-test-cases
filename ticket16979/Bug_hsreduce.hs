{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DeriveGeneric, FlexibleInstances, FunctionalDependencies, GADTs, InstanceSigs, PolyKinds, RankNTypes, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Bug (
        a
    ) where
import Control.Applicative
import Data.Coerce
import Data.Kind
import Data.Monoid
import GHC.Generics
import GHC.TypeLits
data B a b
  = C a (B b a)
  deriving Generic
d = C undefined undefined
a = e (f @0) d
e g = foldrOf g undefined undefined
foldrOf g _ _ = flip appEndo undefined . foldMapOf g undefined
foldMapOf g _ = getConst #. g undefined
class H i where
  dimap :: () -> () -> () -> ()
  (#.) :: Coercible c b => j b c -> i a b -> i a c
instance H (->) where
  dimap _ _ _ = undefined
  {-# INLINE dimap #-}
  (#.) _ = coerce :: forall a b. Coercible b a => a -> b
  {-# INLINE (#.) #-}
class HasParam i k l a b | i l a -> k, i k b -> l, i k -> a, i
                                                             l -> b where
  f :: Applicative g => (a -> g b) -> k -> g l
instance (GenericN k,
          GenericN l,
          k ~ Infer l (M n () 'N) (),
          l ~ Infer k (M n () 'N) b,
          b ~ ArgAt l n,
          GHasParam n (O k) (O l) () b) =>
         HasParam n k l () b where
  f = confusing (\ f k -> p <$> gparam @n f (fromN k))
  {-# INLINE f #-}
confusing l
  = \ f -> lowerYoneda . lowerCurried . l (liftCurriedYoneda . f)
newtype Q f a = Q {r :: forall b. (a -> b) -> f b}
instance Functor (Q f) where
instance Applicative (Q f) where
newtype Curried f a
  = Curried {runCurried :: forall r. f (a -> r) -> f r}
instance Functor (Curried f) where
  fmap _ (Curried g) = Curried (g . undefined)
  {-# INLINE fmap #-}
instance Applicative (Curried f) where
  pure _ = undefined
  {-# INLINE pure #-}
  Curried mf <*> Curried ma = Curried (ma . mf . undefined)
  {-# INLINE (<*>) #-}
lowerYoneda (Q f) = f id
lowerCurried (Curried f) = f (pure id)
liftCurriedYoneda fa = Curried (`s` fa)
s (Q k) fa = undefined (\ _ -> k (undefined .) <*> fa)
class GHasParam i k l a b where
  gparam :: Applicative g => (a -> g b) -> k () -> g (l ())
instance (GHasParam i g l' () (), GHasParam i r r' a b) =>
         GHasParam i (g :*: r) (l' :*: r') a b where
  gparam f (_ :*: r) = (:*:) <$> undefined <*> gparam @i f r
instance GHasParam i k l a b =>
         GHasParam i (M1 () meta k) (M1 () meta l) a b where
  gparam f (M1 t) = M1 <$> gparam @i f t
instance {-# OVERLAPPABLE #-} (GHasParamRec (LookupParam si i) k l a b) =>
                              GHasParam i (Rec si k) (Rec ti l) a b where
  gparam f (Rec (K1 t))
    = undefined <$> gparamRec @(LookupParam si i) f t
class GHasParamRec f k l a b | f l a b -> k, f k a b -> l where
  gparamRec :: Applicative g => (a -> g b) -> k -> g l
instance GHasParamRec 'Nothing a a () () where
instance (HasParam n k l a b) =>
         GHasParamRec ('Just n) k l a b where
  gparamRec = f @n
type family LookupParam (a :: k) (i :: Nat) :: Maybe Nat where
  LookupParam (f (n :: Nat)) m = 'Nothing
  LookupParam (a (_ (m))) n = IfEq m n ('Just 0) (MaybeAdd (LookupParam a n) 1)
  LookupParam () n = MaybeAdd (LookupParam () n) 1
  LookupParam () _ = 'Nothing
type family MaybeAdd a b where
  MaybeAdd 'Nothing _ = 'Nothing
  MaybeAdd ('Just a) b = 'Just (b)
type family IfEq a b l f where
  IfEq a a l _ = l
  IfEq _ _ _ f = f
data Sub where Sub :: Nat -> k -> Sub
type family ReplaceArg (l :: k) (pos :: Nat) (to :: j) :: k where
  ReplaceArg (l ()) 0 to = l to
  ReplaceArg (l ()) pos to = ReplaceArg l (pos - 1) to ()
  ReplaceArg () _ () = ()
type family ReplaceArgs l subs where
  ReplaceArgs l '[] = l
  ReplaceArgs l ('Sub n arg
                 : ss) = ReplaceArgs (ReplaceArg l n arg) ss
type family ArgAt (l :: k) (n :: Nat) :: j where
  ArgAt (l a) 0 = a
  ArgAt (l ()) n = ArgAt l (n - 1)
type family U (a :: k) (b :: k) :: [Sub] where
  U (i n () 'N) a' = '[ 'Sub n a']
  U (a ()) (b ()) = U a b
  U () () = '[]
type family Infer k a' b where
  Infer (k a) a' b = ReplaceArgs (k ()) (U a' b)
  Infer () () () = ()
data N = N
type family M :: Nat -> k -> N -> k
type family Param where
type family Indexed (l :: k) (i :: Nat) :: k where
  Indexed (l a) i = Indexed l (1) (Param i)
  Indexed l _ = l
newtype Rec i a t = Rec {unRec :: K1 () a t}
type family Y a b where
  Y (M1 mt m k) (M1 mt m l) = M1 () m (Y k l)
  Y (g :+: r) (l' :+: r') = Y r r'
  Y (g :*: r) (l' :*: r') = Y g l' :*: Y r r'
  Y (Rec0 i) (Rec0 a) = Rec i a
  Y U1 U1 = U1
class GenericN a where
  type O a :: Type -> Type
  type O a = Y (Rep (Indexed a 0)) (Rep a)
  p :: O a () -> a
  fromN :: a -> O a ()
instance (Coercible (Rep a) (O a), Generic a) => GenericN a where
  p :: O a () -> a
  p = undefined
  {-# INLINE p #-}
  fromN :: forall t. a -> O a t
  fromN = coerce (from :: a -> Rep a t)
  {-# INLINE fromN #-}
