module Merkle.Store where

--------------------------------------------
import           Data.Functor.Compose
import           Data.Kind (Type)
import           Merkle.Functors (HashTagged)
import           Merkle.Types (Hash)
import           Util.MyCompose
import           Util.RecursionSchemes
--------------------------------------------

data Store m (f :: Type -> Type)
  = Store
  { sDeref :: Hash f -> m $ f $ Fix (HashTagged f `Compose` Maybe `Compose` f)
  , sUploadShallow :: AlgebraM m f (Hash f)
  }

uploadDeep
  :: forall m f
   . Traversable f
  => Monad m
  => Store m f
  -> (Fix f)
  -> m (Hash f)
uploadDeep store = cataM (sUploadShallow store)
