{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}


-- | A BUNCH OF SHIT STOLEN FROM COMPDATA + POLYKINDS, TODO UPSTREAM PR
module Util.HRecursionSchemes where

import           Data.Singletons

import Data.Functor.Compose -- todo standardize on my more concise repr here too
import Data.Kind (Type)

type f :-> g = forall (i :: k) . f i -> g i

class HFunctor (h :: (k -> Type) -> k -> Type) where
    hfmap :: (f :-> g) -> h f :-> h g

instance (Functor f) => HFunctor (Compose f) where hfmap f (Compose xs) = Compose (fmap f xs)


-- | This data type represents contexts over a signature. Contexts are
-- terms containing zero or more holes. The first type parameter is
-- supposed to be one of the phantom types 'Hole' and 'NoHole'. The
-- second parameter is the signature of the context. The third
-- parameter is the type family of the holes. The last parameter is
-- the index/label.

data Cxt h f a i where
    Term ::  f (Cxt h f a) i -> Cxt h f a i
    Hole :: a i -> Cxt Hole f a i

-- | Phantom type that signals that a 'Cxt' might contain holes.
data Hole
-- | Phantom type that signals that a 'Cxt' does not contain holes.
data NoHole

-- | A context might contain holes.
type Context = Cxt Hole

-- | A (higher-order) term is a context with no holes.
type Term f = Cxt NoHole f (K ())

-- | This function unravels the given term at the topmost layer.
unTerm :: Term f t -> f (Term f) t
unTerm (Term t) = t

instance (HFunctor f) => HFunctor (Cxt h f) where
    hfmap f (Hole x) = Hole (f x)
    hfmap f (Term t) = Term (hfmap (hfmap f) t)


-- | The parametrised constant functor.
newtype K a i = K {unK :: a} deriving (Functor, Foldable, Traversable)


-- | This type represents multisorted @f@-algebras with a family @e@
-- of carriers.
type Alg f e = f e :-> e

-- | Construct a catamorphism from the given algebra.
cata :: forall f a. HFunctor f => Alg f a -> Term f :-> a
cata f = run
    where run :: Term f :-> a
          run (Term t) = f (hfmap run t)

----------------
-- Coalgebras --
----------------

type Coalg f a = a :-> f a

{-| This function unfolds the given value to a term using the given
unravelling function. This is the unique homomorphism @a -> Term f@
from the given coalgebra of type @a -> f a@ to the final coalgebra
@Term f@. -}

ana :: forall f a. HFunctor f => Coalg f a -> a :-> Term f
ana f = run
    where run :: a :-> Term f
          run t = Term $ hfmap run (f t)

-----------------------------------
-- CV-Coalgebras & Futumorphisms --
-----------------------------------


-- | This type represents cv-coalgebras over functor @f@ and with domain
-- @a@.

type CVCoalg f a = a :-> f (Context f a)


-- | This function constructs the unique futumorphism from the given
-- cv-coalgebra to the term algebra.
futu :: forall f a . HFunctor f => CVCoalg f a -> a :-> Term f
futu coa = ana run . Hole
    where run :: Coalg f (Context f a)
          run (Hole a) = coa a
          run (Term v) = v


-- WEIRD MUTANT SHIT:

-- data Cxt' h f (c :: k -> Constraint) a i where
--     Term' ::  f (Cxt' h f c a) i -> Cxt' h f c a i
--     Hole' :: a i -> Cxt' Hole' f c a i

-- -- | Phantom type that signals that a 'Cxt' might contain holes.
-- data Hole'
-- -- | Phantom type that signals that a 'Cxt' does not contain holes.
-- data NoHole'

-- -- | A context might contain holes.
-- type Context' = Cxt' Hole'

-- -- | A (higher-order) term is a context with no holes.
-- -- type Term' f c = Cxt NoHole f c (K ())

-- type (f :--> g) c = forall (i :: k) . c i => f i -> g i

-- type SCoalg c f a = (a :--> f a) c

-- {-| This function unfolds the given value to a term using the given
-- unravelling function. This is the unique homomorphism @a -> Term f@
-- from the given coalgebra of type @a -> f a@ to the final coalgebra
-- @Term f@. -}

-- sAna :: forall c f a. HFunctor f => SCoalg c f a -> (a :--> Cxt' NoHole' f c (K ())) c
-- sAna f = run
--     where run :: (a :--> Cxt' NoHole' f c (K ())) c
--           run t = Term $ hfmap run (f t)

-- -- | This type represents cv-coalgebras over functor @f@ and with domain
-- -- @a@.

-- type SCVCoalg c f a = (a :--> f (Context f a)) c


-- -- | This function constructs the unique futumorphism from the given
-- -- cv-coalgebra to the term algebra.
-- sFutu :: forall c f a . HFunctor f => SCVCoalg c f a -> ((a :--> Cxt' f c (K ())) c)
-- sFutu coa = sAna @c run . Hole
--     where run :: SCoalg c f (Context f a)
--           run (Hole a) = coa a
--           run (Term v) = v



-- idea: sing instead of specialized to any constraint, just constrain to Sing

-- | A (higher-order) term is a context with no holes.
-- type Term' f c = Cxt NoHole f c (K ())


type f :--> g = forall (i :: k) . SingI i => f i -> g i


class SHFunctor (h :: (k -> Type) -> k -> Type) where
    shfmap :: (f :--> g) -> h f :--> h g


type SCoalg f a = a :--> f a

{-| This function unfolds the given value to a term using the given
unravelling function. This is the unique homomorphism @a -> Term f@
from the given coalgebra of type @a -> f a@ to the final coalgebra
@Term f@. -}

sAna :: forall f a. SHFunctor f => SCoalg f a -> (a :--> Term f)
sAna f = run
    where run :: a :--> Term f
          run t = Term $ shfmap run (f t)

-- | This type represents cv-coalgebras over functor @f@ and with domain
-- @a@.

type SCVCoalg f a = a :--> f (Context f a)


-- | This function constructs the unique futumorphism from the given
-- cv-coalgebra to the term algebra.
sFutu :: forall f a . SHFunctor f => SCVCoalg f a -> a :--> Term f
sFutu coa = sAna run . Hole
    where run :: SCoalg f (Context f a)
          run (Hole a) = coa a
          run (Term v) = v

instance (Functor f) => SHFunctor (Compose f)
  where shfmap f (Compose xs) = Compose (fmap f xs)

instance (SHFunctor f) => SHFunctor (Cxt h f) where
    shfmap f (Hole x) = Hole (f x)
    shfmap f (Term t) = Term (shfmap (shfmap f) t)
