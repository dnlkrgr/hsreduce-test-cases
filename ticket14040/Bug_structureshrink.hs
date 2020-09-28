{-#LANGUAGE AllowAmbiguousTypes#-}{-#LANGUAGE ConstraintKinds#-}{-#LANGUAGE ScopedTypeVariables#-}{-#LANGUAGE TypeApplications#-}{-#LANGUAGE TypeFamilies#-}{-#LANGUAGE TypeInType#-}{-#LANGUAGE TypeOperators#-}module E where
import Data.Kind
data(>)
class T(ar::w)where type Fn(b)ar(p)
class T ar=>A(ar)where type App b ar b'(f::Fn b ar b')(a::b)::b'
type F ar=T ar
instance T(>)where type Fn b(>)b'=b->b'
instance A(>)where type App b(>)b'(f::b->b')a=f a
type(-)b(b'::Type)ar=Fn b ar b'
l::forall(p::Type)(l::a).p->p
l=i @(>) @a @l
i::forall(a::Type)(p::([a]-Type)ar)(l::[a]).F a=>App[a]ar Type p '[]->App[a]ar Type p l
i=p