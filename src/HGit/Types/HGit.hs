module HGit.Types.HGit where

--------------------------------------------
import qualified Data.Aeson as AE
import           Data.ByteString (ByteString)
import           Data.List (sortOn)
import           Data.List.NonEmpty
import           Control.Monad (join)
import           GHC.Generics (Generic1)
--------------------------------------------
import           HGit.Types.Common
import           Merkle.Types
--------------------------------------------

data Blob a
  = Chunk String a
  | Empty
  -- NOTE: using String instead of Bytestring to allow for easy examination of serialized files
  deriving (Eq, Ord, Functor, Foldable, Traversable, Generic1)


instance AE.ToJSON1 Blob
instance AE.FromJSON1 Blob


data Dir a b = Dir { dirEntries :: [NamedFileTreeEntity a b] }
  deriving (Eq, Ord, Functor, Foldable, Traversable, Generic1)


instance AE.ToJSON1 (Dir (Hash (Blob)))
instance AE.FromJSON1 (Dir (Hash (Blob)))


data Commit a b = NullCommit | Commit String a (NonEmpty b)
  deriving  (Eq, Ord, Functor, Foldable, Traversable, Generic1)

instance AE.ToJSON1 (Commit (Hash (Dir (Hash Blob))))
instance AE.FromJSON1 (Commit (Hash (Dir (Hash Blob))))

-- | sort dir here by file name, order is irrelevant
canonicalOrdering :: [NamedFileTreeEntity a b] -> [NamedFileTreeEntity a b]
canonicalOrdering = sortOn fst

data FileTreeEntity a b
  = FileEntity a -- file type
  | DirEntity  b -- continued directory structure type
  deriving (Eq, Ord, Functor, Foldable, Traversable, Generic1)


instance AE.ToJSON1 (FileTreeEntity (Hash (Blob)))
instance AE.FromJSON1 (FileTreeEntity (Hash (Blob)))

-- todo: literally just bifunctor/bifoldable
fte :: (a -> c)
    -> (b -> c)
    -> FileTreeEntity a b
    -> c
fte f _ (FileEntity x) = f x
fte _ g (DirEntity  x) = g x

ftem :: (a -> b)
     -> (c -> d)
     -> FileTreeEntity a c
     -> FileTreeEntity b d
ftem f _ (FileEntity x) = FileEntity $ f x
ftem _ g (DirEntity  x) = DirEntity $ g x


type NamedFileTreeEntity a b
  = ( PartialFilePath -- name of this directory entry (files and dirs have same name rules)
    , FileTreeEntity a b
    )

instance Hashable Blob where
  -- file-type entities
  hash (Chunk chunk next) = doHash $ ["blob", unpackString chunk, unpackHash next]
  hash (Empty) = emptyHash

instance Hashable (Dir (Hash Blob)) where
  hash (Dir []) = emptyHash
  -- non-empty dir-type entities
  hash (Dir xs) = doHash $ ["dir" :: ByteString] ++ join (fmap hashNFTE $ canonicalOrdering xs)
    where hashNFTE (name, f) = [unpackString name] ++ hashFTE f
          hashFTE =
            fte (\(chp :: Hash Blob) -> ["subfile", unpackHash chp])
                (\(chp :: Hash (Dir (Hash Blob)))  -> ["subdir", unpackHash chp])

instance Hashable (Commit (Hash (Dir (Hash Blob)))) where
  -- commit-type entities
  hash NullCommit = emptyHash
  hash (Commit msg root parents)
    = doHash $
        [ unpackString msg
        , unpackHash root
        ] ++ toList (fmap unpackHash parents)
