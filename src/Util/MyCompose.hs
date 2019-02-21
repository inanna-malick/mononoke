-- | in this module we commit the cardinal haskell sin...
--   ...defining our own type-level operators for convenience...
module Util.MyCompose where


import Control.Applicative
import qualified Data.Comp.Multi.HFunctor as C

data (f :+ g) a = C (f (g a))
infixr 7 :+

getCompose :: (f :+ g) a -> f (g a)
getCompose (C x) = x

-- all this copied from transformers pkg
instance (Functor f, Functor g) => Functor (f :+ g) where
    fmap f (C x) = C (fmap (fmap f) x)

instance (Foldable f, Foldable g) => Foldable (f :+ g) where
    foldMap f (C t) = foldMap (foldMap f) t

instance (Traversable f, Traversable g) => Traversable (f :+ g) where
    traverse f (C t) = C <$> traverse (traverse f) t

instance (Applicative f, Applicative g) => Applicative (f :+ g) where
    pure x = C (pure (pure x))
    C f <*> C x = C ((<*>) <$> f <*> x)

instance (Alternative f, Applicative g) => Alternative (f :+ g) where
    empty = C empty
    C x <|> C y = C (x <|> y)


-- taken from http://hackage.haskell.org/package/type-operators-0.1.0.4/docs/src/Control-Type-Operator.html#%24
-- | Infix application.
--
-- @
-- f :: Either String $ Maybe Int
-- =
-- f :: Either String (Maybe Int)
-- @
type f $ a = f a
infixr 2 $


-- stolen from compdata w/ missing instances added 420 yolo

data (:++) f g e t = HC (f (g e) t)
infixr 7 :++

getHCompose :: (f :++ g) e t -> f (g e) t
getHCompose (HC x) = x

instance (C.HFunctor f, C.HFunctor g) => C.HFunctor ((:++) f g) where
  hfmap f (HC x) = HC $ C.hfmap (C.hfmap f) x
