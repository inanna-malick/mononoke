module Merkle.Store where

--------------------------------------------
import           Data.Kind
import           Merkle.Tree.Types
import           Util.MyCompose
import           Util.HRecursionSchemes
--------------------------------------------
import qualified Data.Functor.Compose as FC
import           Data.Functor.Const
import           Data.Singletons (SingI)

data Store m (f :: (k -> Type) -> k -> Type)
  = Store
  {
    sDeref :: forall i . SingI i => HashPointer -> m $ f (Term (FC.Compose HashIndirect :++ f)) i
  , sUploadShallow :: forall i. SingI i => f (Const HashPointer) i -> m HashPointer
  }
