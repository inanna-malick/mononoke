{-# LANGUAGE TemplateHaskell #-}

module HGit.Core.Types where

--------------------------------------------
import           Control.Applicative (Const(..))
import qualified Data.Aeson as AE
import           Data.Bifunctor.TH
import           Data.Eq.Deriving
import           Data.Functor.Compose
import           Data.List.NonEmpty (NonEmpty, toList)
import qualified Data.Map.Strict as Map
import           GHC.Generics
import           Text.Show.Deriving
--------------------------------------------
import qualified Merkle.Types as MT
import qualified Merkle.Functors as MF
import qualified Merkle.Store as MS
import           Merkle.Types.IPFS
import           Util.RecursionSchemes
--------------------------------------------

-- | Using IPFS as a store for this project, specialize a bunch of types to that hash
type Hash = MT.Hash RawIPFSHash
type HashAnnotated f = MF.HashAnnotated RawIPFSHash f
type PutCapability m f = MS.PutCapability m RawIPFSHash f
type GetCapability m f = MS.GetCapability m RawIPFSHash f
type Store m f = MS.Store m RawIPFSHash f
type ShallowStore m f = MS.ShallowStore m RawIPFSHash f

unGetCapability
  :: GetCapability m f
  -> Hash f -> m (Maybe (MS.DerefRes RawIPFSHash f))
unGetCapability (MS.GetCapability g) = g

unPutCapability
  :: PutCapability m f
  -> f (Hash f) -> m (Hash f)
unPutCapability (MS.PutCapability p) = p

type PartialFilePath = String
type BranchName      = String
type CommitMessage   = String

data Blob a
  -- NOTE: using String instead of Bytestring to allow for easy examination of serialized files
  = Chunk String a
  | Empty
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic1)

instance ExtractKeys Blob where
  extractRawKeys (Chunk _x (Const rh)) = [rh]
  extractRawKeys  Empty                = []

instance AE.ToJSON1 Blob
instance AE.FromJSON1 Blob

data FileTreeEntity a b
  = FileEntity a -- file type
  | DirEntity  b -- continued directory structure type
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic1)

$(deriveBifoldable    ''FileTreeEntity)
$(deriveBifunctor     ''FileTreeEntity)
$(deriveBitraversable ''FileTreeEntity)


$(deriveShow1 ''FileTreeEntity)
$(deriveShow2 ''FileTreeEntity)
$(deriveEq2   ''FileTreeEntity)
$(deriveEq1   ''FileTreeEntity)

instance AE.ToJSON1 (FileTreeEntity (Hash (Blob)))
instance AE.FromJSON1 (FileTreeEntity (Hash (Blob)))

type NamedFileTreeEntity a b
  = ( PartialFilePath -- name of this directory entry (files and dirs have same name rules)
    , FileTreeEntity a b
    )

data Dir a b = Dir { dirEntries :: [NamedFileTreeEntity a b] }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic1)

type HashableDir = Dir (Hash Blob)

instance ExtractKeys HashableDir where
  extractRawKeys (Dir xs) =
    let f (_n, FileEntity (Const rh)) = rh
        f (_n, DirEntity  (Const rh)) = rh
     in fmap f xs

-- | A lazy structure in which each effectful fetch action is annotated with the
-- hash of the structure to be fetched. For use with hash-addressed stores.
type LazyMerkle f m = Fix (HashAnnotated f `Compose` m `Compose` f)

type LazyMerkleDir m x = Fix (HashAnnotated (Dir x) `Compose` m `Compose` Dir x)

$(deriveBifoldable    ''Dir)
$(deriveBifunctor     ''Dir)
$(deriveBitraversable ''Dir)

$(deriveShow1 ''Dir)
$(deriveShow2 ''Dir)
$(deriveEq2   ''Dir)
$(deriveEq1   ''Dir)

instance AE.ToJSON1 (Dir (Hash (Blob)))
instance AE.FromJSON1 (Dir (Hash (Blob)))


data Commit a b = NullCommit | Commit String a (NonEmpty b)
  deriving  (Eq, Ord, Functor, Foldable, Traversable, Generic1)

$(deriveBifoldable    ''Commit)
$(deriveBifunctor     ''Commit)
$(deriveBitraversable ''Commit)

$(deriveShow1 ''Commit)
$(deriveShow2 ''Commit)
$(deriveEq2   ''Commit)
$(deriveEq1   ''Commit)

type HashableCommit = Commit (Hash HashableDir)

instance ExtractKeys HashableCommit where
  extractRawKeys  NullCommit = []
  extractRawKeys (Commit _msg (Const rh) rhs) = [rh] ++ (fmap getConst $ toList rhs)


instance AE.ToJSON1   HashableCommit
instance AE.FromJSON1 HashableCommit

-- | sort dir here by file name, specific order is irrelevant
canonicalOrdering :: [NamedFileTreeEntity a b] -> [NamedFileTreeEntity a b]
canonicalOrdering = Map.toList . Map.fromList
