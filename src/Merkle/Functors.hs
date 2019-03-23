module Merkle.Functors where

--------------------------------------------
import           Data.Functor.Compose
--------------------------------------------
import           Merkle.Types
import           Util.RecursionSchemes
--------------------------------------------

type HashTagged f = (,) (Hash f)

htPointer
  :: Fix (HashTagged x `Compose` f)
  -> Hash x
htPointer (Fix (Compose (p, _))) = p

htElem
  :: Fix (HashTagged x `Compose` f)
  -> f (Fix (HashTagged x `Compose` f))
htElem (Fix (Compose (_, e))) = e

-- | Remove hash annotations from some HashTagged structure
stripTags :: Functor f => Fix (HashTagged x `Compose` f) -> Fix f
stripTags = cata (Fix . snd . getCompose)

-- | Annotate each layer of some structure with its hash
hashTag
  :: Functor f
  => Hashable f
  => Fix f
  -> Fix (HashTagged f `Compose` f)
hashTag = annotate hash
