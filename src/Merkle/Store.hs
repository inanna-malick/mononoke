module Merkle.Store where

--------------------------------------------
import           Data.Kind
import           Merkle.Types (HashPointer, HashIndirect)
import           Util.HRecursionSchemes
--------------------------------------------

data Store m (f :: (k -> Type) -> k -> Type)
  = Store
  { sDeref :: NatM m (Const HashPointer) (f (Term (HashIndirect f)))
  , sUploadShallow :: AlgM m f (Const HashPointer)
  }
