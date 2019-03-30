module Merkle.Store where

--------------------------------------------
import           Data.Functor.Compose
import           Data.Kind (Type)
import           Merkle.Functors (HashTagged)
import           Merkle.Types (Hash)
import           Util.RecursionSchemes
--------------------------------------------

type DerefRes f = f (Fix (HashTagged f `Compose` Maybe `Compose` f))

data Store m (f :: Type -> Type)
  = Store
  { sDeref :: Hash f -> m (DerefRes f)
  , sUploadShallow :: f (Hash f) -> m (Hash f)
  }

uploadDeep
  :: forall m f
   . Traversable f
  => Monad m
  => Store m f
  -> (Fix f)
  -> m (Hash f)
uploadDeep store = cataM (sUploadShallow store)

liftStore :: (forall x. m x -> m' x) -> Store m f -> Store m' f
liftStore f (Store d u) = Store (f . d) (f . u)

-- derefWithFallback :: Store m f -> Store m f -> (Hash f -> m (Maybe (DerefRes f)))
-- derefWithFallback (Store d1 _) (Store d2 _) = do
--   deref1Res 


{-
-- IDEA: distinct shallow store type (for single-layer deref caps, can be lifted)
data Store m (f :: Type -> Type)
  = Store
  { sDeref :: Hash f -> m $ f $ Fix (HashTagged f `Compose` Maybe `Compose` f)
  , sUpload :: Fix f -> m (Hash f)
  }

data ShallowStore m (f :: Type -> Type)
  = Store
  { sDerefShallow :: Hash f -> m $ f $ Fix (HashTagged f `Compose` Maybe `Compose` f)
  , sUploadShallow :: AlgebraM m f (Hash f)
  }
-}
