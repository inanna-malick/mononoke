module Merkle.Store where

--------------------------------------------
import           Merkle.Types
import           Util.MyCompose
import           Util.RecursionSchemes
--------------------------------------------

data Store m x
  = Store
  { -- | given a pointer, fetch the corresponding entity. Provides type-level guarantee that
    --   at least one level of structure is fetched 'x' while allowing for multiple
    --   levels of structure to be returned in one call via 'Merkle' subnode type
    sDeref :: Pointer -> m $ x $ Fix (WithHash :+ Maybe :+ x)
    -- | given a shallow layer of structure with subnodes identified by a pointer, store it.
    -- this allows for each store to use its own hash algorithm - not sure if I like that
  , sUploadShallow :: x $ Pointer -> m Pointer
  }


-- | consume effectful tree, annotate nodes with hash,
--   adds them to some global store during this traversal
addToStore
  :: forall m x
   . Monad m
  => Traversable x
  => Store m x
  -> Fix $ m :+ x
  -> m $ Fix $ WithHash :+ x
addToStore store = cata alg
  where
    alg :: Algebra (m :+ x) (m $ Fix $ WithHash :+ x)
    alg (C effect) = do
      entity <- effect >>= traverse id
      p <- sUploadShallow store . fmap pointer $ entity
      pure . Fix . C $ (p, entity)
