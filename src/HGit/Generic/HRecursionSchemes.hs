{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- | mostly sourced from COMPDATA, with added POLYKINDS/SING stuff from me, TODO UPSTREAM or use original
module HGit.Generic.HRecursionSchemes where

--------------------------------------------
import           Data.Functor.Compose
import           Data.Kind (Type)
import           Data.Singletons
import qualified Data.Singletons.TH
--------------------------------------------


-- | NOTE: the below 3 functions are the only significant divergence from compdata
type NatM m f g = forall i. SingI i => f i -> m (g i)
type f :-> g = forall i . SingI i => f i -> g i
type f :=> a = forall i . SingI i => f i -> a

class HFunctor (h :: (k -> Type) -> k -> Type) where
    hfmap :: (f :-> g) -> h f :-> h g


data Uninhabited a

instance (Functor f) => HFunctor (Compose f) where
  hfmap f (Compose xs) = Compose (fmap f xs)

-- note: just the bit I need for hcataM/anaM
class HTraversable t where
    hmapM :: (Monad m) => NatM m f g -> NatM m (t f) (t g)
    htraverse :: Applicative f => NatM f a b -> NatM f (t a) (t b)

instance (Traversable f) => HTraversable (Compose f) where
  hmapM nat (Compose xs) = Compose <$> traverse nat xs
  htraverse nat (Compose xs)= Compose <$> traverse nat xs

$(Data.Singletons.TH.singletons [d| data CxtType = WithHole | WithNoHole |])

data Cxt (h :: CxtType)
         (f :: (k -> Type) -> k -> Type)
         (a :: k -> Type)
         (i :: k) where
    Term ::  f (Cxt h f a) i -> Cxt h f a i
    Hole :: a i -> Cxt 'WithHole f a i


type Context = Cxt 'WithHole
type Term f = Cxt 'WithNoHole f Uninhabited

unTerm :: Term f t -> f (Term f) t
unTerm (Term t) = t

instance (HFunctor f) => HFunctor (Cxt h f) where
    hfmap f (Hole x) = Hole (f x)
    hfmap f (Term t) = Term (hfmap (hfmap f) t)

type Alg f e = f e :-> e

hcata :: forall f a. HFunctor f => Alg f a -> Term f :-> a
hcata f = f . hfmap (hcata f) . unTerm

type RAlg f a = f (Term f `Tagged` a) :-> a

-- | This function constructs a paramorphism from the given r-algebra
hpara :: forall f a. (HFunctor f) => RAlg f a -> Term f :-> a
hpara f = _elem . hcata run
    where run :: Alg f  (Term f `Tagged` a)
          run t = Term (hfmap _tag t) `Tagged` f t

type AlgM m f e = NatM m (f e) e

-- | This is a monadic version of 'hcata'.
hcataM
  :: forall f m a
   . (HTraversable f, Monad m)
  => AlgM m f a
  -> NatM m (Term f) a
hcataM f = (>>= f) . hmapM (hcataM f) . unTerm

type Coalg f a = a :-> f a

ana :: forall f a. HFunctor f => Coalg f a -> (a :-> Term f)
ana f = Term . hfmap (ana f) . f


type CoalgPartial f a = a :-> HEither (f a) a

anaPartial
  :: forall f a
   . HFunctor f
  => CoalgPartial f a
  -> (a :-> Context f a)
anaPartial f = helper . f
  where
    helper (L fi) = Term $ hfmap (anaPartial f) fi
    helper (R gi) = Hole gi

type CoalgM m f a = NatM m a (f a)

anaM
  :: forall a m f
   . (HTraversable f, Monad m)
  => CoalgM m f a
  -> NatM m a (Term f)
anaM f = fmap Term . (>>= hmapM (anaM f)) . f


type CoalgPartialM m f a = NatM m a (HEither (f a) a)

anaPartialM
  :: forall m f a
   . (HTraversable f, Monad m)
  => CoalgPartialM m f a
  -> NatM m a (Context f a)
anaPartialM f a = f a >>= helper
   where
    helper :: NatM m (HEither (f a) a) (Context f a)
    helper (L fi) = Term <$> hmapM (anaPartialM f) fi
    helper (R gi) = pure $ Hole gi


unCxt :: f (Cxt h f a) :-> x
      -> a :-> x
      -> Cxt h f a :-> x
unCxt f _ (Term x) = f x
unCxt _ f (Hole x) = f x

unCxtM :: NatM m (f (Cxt h f a)) x
      -> NatM m a x
      -> NatM m (Cxt h f a) x
unCxtM f _ (Term x) = f x
unCxtM _ f (Hole x) = f x


-- | Computation yielding partial Term with Hole
type CVCoalg f a = a :-> (f (Context f a))

futa :: forall f a . HFunctor f => CVCoalg f a -> a :-> Term f
futa coa = ana (unCxt id coa) . Hole


-- | Monadic computation yielding partial Term with Hole
type CVCoalgM m f a = NatM m a (f (Context f a))


-- | Higher Order Monadic Futamorphism
futaM
  :: forall m f a
   . (HTraversable f, Monad m)
  => CVCoalgM m f a
  -> NatM m a (Term f)
futaM coa = anaM (unCxtM pure coa) . Hole


-- | ETC:

data Tagged f g i = Tagged { _tag :: (f i), _elem :: (g i) }

instance HFunctor (Tagged x) where
    hfmap f (Tagged x y) = Tagged x (f y)

instance HTraversable (Tagged x) where
  hmapM nat (Tagged x y) = Tagged x <$> nat y
  htraverse nat (Tagged x y)= Tagged x <$> nat y

data HCompose f g e t = HC { getHC :: (f (g e) t) }
infixr 7 `HCompose`

instance (HFunctor f, HFunctor g) => HFunctor (f `HCompose` g) where
  hfmap f (HC x) = HC $ hfmap (hfmap f) x

data HEither f g i
  = L (f i)
  | R (g i)
infixr 7 `HEither`

instance HFunctor (HEither f) where
  hfmap _ (L x) = L x
  hfmap f (R x) = R $ f x

instance (HTraversable f, HTraversable g) => HTraversable (f `HCompose` g) where
  hmapM nat (HC x) = HC <$> hmapM (hmapM nat) x
  htraverse nat (HC x)= HC <$> htraverse (htraverse nat) x

