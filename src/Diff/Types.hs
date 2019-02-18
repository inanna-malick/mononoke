module Diff.Types where

--------------------------------------------
import           Merkle.Tree.Types (Name)
import           Merkle.Tree.Encoding (ShallowMerkleTreeLayer)
--------------------------------------------

type FileBody = String

data Diff = LeafModified Name ShallowMerkleTreeLayer ShallowMerkleTreeLayer
          | FileReplacedWithDir Name
          | DirReplacedWithFile Name
          | EntityAddedToDir
          | EntityRemovedFromDir
          | EntityRenamed Name Name
          | EntityDeleted Name
          | EntityCreated Name
  deriving (Show)
