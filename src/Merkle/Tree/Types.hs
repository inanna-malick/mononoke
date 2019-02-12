module Merkle.Tree.Types where

-- | Tree in which leaf nodes are specialized to String
data Tree a = Node [a] | Leaf String deriving (Eq, Show, Functor, Foldable, Traversable)

type Name = String

-- | Entity tagged with a name
type Named = (,) Name
