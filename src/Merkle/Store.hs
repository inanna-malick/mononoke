module Merkle.Store where

--------------------------------------------
import qualified Data.Functor.Compose as FC
import           Data.Functor.Const
import           Data.Singletons (SingI)
--------------------------------------------
import           Data.Kind
import           Merkle.Types (HashPointer, HashIndirect)
import           Util.MyCompose
import           Util.HRecursionSchemes
--------------------------------------------

data Store m (f :: (k -> Type) -> k -> Type)
  = Store
  {
    sDeref :: forall i . SingI i => Const HashPointer i -> m $ f (Term (FC.Compose HashIndirect :++ f)) i
  , sUploadShallow :: forall i. SingI i => f (Const HashPointer) i -> m (Const HashPointer i)
  }
