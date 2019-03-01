module Diff.Types where

--------------------------------------------
import           HGit.Types (PartialFilePath, HashPointer)
--------------------------------------------

type FileBody = String

data Diff = LeafModified  (PartialFilePath, HashPointer, HashPointer)
          | FileReplacedWithDir PartialFilePath
          | DirReplacedWithFile PartialFilePath
          | EntityAddedToDir
          | EntityRemovedFromDir
          | EntityRenamed PartialFilePath PartialFilePath
          | EntityDeleted PartialFilePath
          | EntityCreated PartialFilePath
  deriving (Show)
