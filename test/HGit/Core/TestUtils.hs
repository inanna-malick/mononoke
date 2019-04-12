module HGit.Core.TestUtils where

--------------------------------------------
import           Control.Monad.Fail (MonadFail)
import           Data.Functor.Compose
--------------------------------------------
import           Data.Aeson.Orphans ()
import           HGit.Core.Types
import           Merkle.Functors
import           Merkle.Types
import           Util.RecursionSchemes
--------------------------------------------


ffail :: MonadFail m => Hash x -> Fix (HashAnnotated x `Compose` m `Compose` x)
ffail h = Fix $ Compose (h, Compose $ fail "Boom!")


dir :: Applicative m
    => [NamedFileTreeEntity (Hash Blob) (LazyMerkleDir m (Hash Blob))]
    -> LazyMerkleDir m (Hash Blob)
dir xs =
  let d = Dir xs
      h = hash $ fmap htPointer d
    in Fix $ Compose (h, Compose $ pure d)

dir'
  :: Applicative m
  => PartialFilePath
  -> [NamedFileTreeEntity (Hash Blob) (LazyMerkleDir m (Hash Blob))]
  -> (PartialFilePath, FileTreeEntity a (LazyMerkleDir m (Hash Blob)))
dir' n xs = (n,) . DirEntity $ dir xs

blobHash :: String -> Hash Blob
blobHash body = hash $ Chunk body emptyHash

file :: PartialFilePath -> String -> (PartialFilePath, FileTreeEntity (Hash Blob) b)
file n = (n,) . FileEntity . blobHash
