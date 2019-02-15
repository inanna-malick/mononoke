module Merkle.Tree.Types where

import Util.MyCompose
import Util.RecursionSchemes

-- | Tree in which leaf nodes are specialized to String
data Tree x a = Node [a] | Leaf x
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- todo idk bytestring or w/e
-- NOTE: this format makes it so looking up file contents is always an
-- additional call - can use that hash indirection trick to sometimes
-- return more of the body, tho.. can also just end merkle diff compare
-- at different file body pointers..
type MyTreeLayer f = f :+ Named :+ Tree (Fix $ f :+ Tree FileChunk)

type Name = String

type FileChunk = String

-- | Entity tagged with a name
type Named = (,) Name
