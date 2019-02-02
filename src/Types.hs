{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Types where

--------------------------------------------
import qualified Data.Hashable as Hash
import qualified Data.HashMap.Strict as Map
import           Data.HashMap.Strict (HashMap)
import           Data.IORef
import           GHC.Generics (Generic)
--------------------------------------------
import           RecursionSchemes (Term(..))
--------------------------------------------

type Hash = Int
type Name = String
type FileBody = String

-- | merkle tree - can be indirect or concrete
-- TODO: name = MerkleTreePointer - maybe?
type MerkleTree = Term (HashIdentifiedEntity (NamedEntity Tree))
-- | merkle tree layer with sub-nodes of type a
type MerkleTreeLayer a = NamedEntity Tree a
-- | merkle tree in which the top layer is known to be substantiated and sub-nodes can be either hash-id'd or direct
type ConcreteMerkleTreeLayer = MerkleTreeLayer MerkleTree


-- | no subtree nodes substantiated, only pointers
type ShallowMerkleTreeLayer = MerkleTreeLayer Pointer


type GlobalStore = IORef (HashMap Pointer ConcreteMerkleTreeLayer)

-- | Pointer to a hash-identified merkle tree node
newtype Pointer = Pointer { unPointer :: Hash }
  deriving (Eq, Ord, Show, Generic)
instance Hash.Hashable Pointer

-- | Named entity
data NamedEntity (f :: * -> *) a
  = NamedEntity
  { neName   :: Name
  , neEntity :: f a
  } deriving (Eq, Show, Functor)

-- | Tree in which leaf nodes are specialized to String
data Tree (a :: *) = Node [a] | Leaf String deriving (Eq, Show, Functor)

-- | Some entity (f a) identified by a hash pointer. Can either be a direct or indirect reference
data HashIdentifiedEntity (f :: * -> *) (a :: *)
  = Direct   Pointer (f a) -- node id is included in node metadata of direct ref (pointer)
  | Indirect Pointer       -- indirect ref is just a pointer in some hash-addressed store
  deriving (Eq, Show, Functor)

mtPointer :: MerkleTree -> Pointer
mtPointer (In (Direct p _)) = p
mtPointer (In (Indirect p)) = p

-- | hash to get pointer, basically - builder fn that takes a flat single layer of a tree
--   and lifts it to produce a direct reference to a pointer-identified tree layer
-- TODO RENAME
lift :: MerkleTreeLayer Pointer -> (Pointer, ConcreteMerkleTreeLayer)
lift e = (Pointer $ hashTree e, (fmap (In . Indirect) e))

-- hash a single merkle tree entry with all subentities represented as hash pointers
hashTree :: MerkleTreeLayer Pointer -> Hash
hashTree (NamedEntity n (Leaf contents))
  = Hash.hash n `Hash.hashWithSalt` Hash.hash contents
hashTree (NamedEntity n (Node contents))
  = Hash.hash n `Hash.hashWithSalt` Hash.hash contents


data Diff = LeafModified  (Name, FileBody, FileBody)
          | FileReplacedWithDir Name
          | DirReplacedWithFile Name
          | EntityAddedToDir
          | EntityRemovedFromDir
          | EntityRenamed Name Name
          | EntityDeleted Name
          | EntityCreated Name
  deriving (Show)

mapCompare :: Eq k => Hash.Hashable k => HashMap k v -> HashMap k v -> [These (k,v)]
mapCompare h1 h2 = h1Only ++ h2Only ++ both
  where h1Only = fmap This . Map.toList $ Map.difference h1 h2
        h2Only = fmap That . Map.toList $ Map.difference h2 h1
        both   = Map.elems $ Map.intersectionWithKey (\k v1 v2 -> These (k,v1) (k,v2)) h1 h2

-- | specialized to 'a a' to make functor derive easy... could do bifunctor?
data These a = This a | These a a | That a deriving (Eq, Ord, Show, Functor)
