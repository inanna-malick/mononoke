module Merkle.Store.Deref where

--------------------------------------------
import           Data.Functor.Compose
--------------------------------------------
import           Util.RecursionSchemes
import           Merkle.Functors
import           Merkle.Store
import           Merkle.Types
--------------------------------------------

strictDeref
  :: Traversable f
  => Monad m
  => Fix (HashTagged f `Compose` m `Compose` f)
  -> m (Fix (HashTagged f `Compose` f))
strictDeref = anaM alg
  where
    alg (Fix (Compose (p, Compose eff))) = do
      x <- eff
      pure $ Compose (p, x)


-- | construct a potentially-infinite tree-shaped stream of further values constructed by
-- deref-ing hash pointers using a hash-addressed store. Allows for store returning multiple
-- layers of tree structure in a single response (to enable future optimizations) via 'CoAttr'
lazyDeref
  :: forall m f
   . Monad m
  => Functor f
  => Store m f
  -> Hash f
  -> Fix (HashTagged f `Compose` m `Compose` f)
lazyDeref store = futu alg
  where
    alg :: CVCoAlgebra (HashTagged f `Compose` m `Compose` f) (Hash f)
    alg p = Compose . (p,) . Compose $ fmap (cata helper) <$> sDeref store p

    helper :: Algebra (HashTagged f `Compose` Maybe `Compose` f)
                      (Free (HashTagged f `Compose` m `Compose` f) (Hash f))
    helper (Compose (p, (Compose Nothing))) = Pure p
    helper (Compose (p, (Compose(Just x))))
      = Free . Compose $ (p, (Compose $ pure $ x))
