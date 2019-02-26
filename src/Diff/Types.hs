module Diff.Types where

--------------------------------------------
import           Merkle.Tree.Types (Name, HashPointer)
--------------------------------------------

type FileBody = String

data Diff = LeafModified  (Name, HashPointer, HashPointer)
          | FileReplacedWithDir Name
          | DirReplacedWithFile Name
          | EntityAddedToDir
          | EntityRemovedFromDir
          | EntityRenamed Name Name
          | EntityDeleted Name
          | EntityCreated Name
  deriving (Show)
