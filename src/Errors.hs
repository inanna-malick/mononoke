module Errors (MerkleTreeLookupError(..), FileReadError(..))where

--------------------------------------------
import           Merkle.Tree.Types (HashPointer(..))
--------------------------------------------

data MerkleTreeLookupError
  = EntityNotFoundInStore HashPointer
  deriving Show

data FileReadError
  = FileReadError FilePath -- tried to read this path but failed (todo better errors? idk lol)
  deriving Show
