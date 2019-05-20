module HGit.Core.Merge where

--------------------------------------------
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map
import           Data.Functor.Compose
--------------------------------------------
import           HGit.Core.Types
import           Merkle.Functors
import           Util.RecursionSchemes
--------------------------------------------

commitMerge
  :: forall m x
  -- no knowledge about actual monad stack - just knows it can
  -- sequence actions in it to deref successive layers (because monad)
   . (Monad m, Eq x)
  => PutCapability m (Dir x)
  -> MergeResult m x
  -> m (LazyMerkleDir m x)
commitMerge put = cataM alg
  where
    alg (Compose (Left  lmd)) = pure lmd
    alg (Compose (Right dir)) = do
      dirHash <- unPutCapability put $ fmap htPointer dir
      pure $ Fix $ Compose $ (dirHash,) $ Compose $ pure $ dir


-- | Merge two merkle trees and either fail (if partial derefing detects a conflict)
--   or succeed (in the case that all differences are non-overlapping)
mergeMerkleDirs
  :: forall m x
  -- no knowledge about actual monad stack - just knows it can
  -- sequence actions in it to deref successive layers (because monad)
   . (Monad m, Eq x)
  => LazyMerkleDir m x
  -> LazyMerkleDir m x
  -> m (MergeViolation `Either` MergeResult m x)
mergeMerkleDirs = (runExceptT .) . mergeDirs []
  where
    mergeDirs
      :: [PartialFilePath]
      -> LazyMerkleDir m x
      -> LazyMerkleDir m x
      -> ExceptT MergeViolation m (MergeResult m x)
    mergeDirs h dir1 dir2 = do
      if htPointer dir1 == htPointer dir2
          -- if both htPointers == then they're identical, either is fine for merge res
          then pure $ Fix $ Compose $ Left dir1
          else do
            ns1 <- lift . fmap dirEntries . getCompose $ htElem dir1
            ns2 <- lift . fmap dirEntries . getCompose $ htElem dir2

            -- merge, preserving non-conflicting changes.
            (entries :: Map.Map PartialFilePath (FileTreeEntity x (MergeResult m x)))
                <- Map.mergeA preserveMissing
                              preserveMissing
                             (Map.zipWithAMatched $ onConflict h)
                             (Map.fromList ns1)
                             (Map.fromList ns2)

            let dir :: Dir x (MergeResult m x)
                dir = Dir $ canonicalOrdering $ Map.toList entries

            pure $ Fix $ Compose $ Right dir

    preserveMissing = Map.mapMissing preserveMissing'

    preserveMissing' _k (DirEntity  x) = DirEntity  $ Fix $ Compose $ Left x
    preserveMissing' _k (FileEntity x) = FileEntity x

    -- two files with the same path
    onConflict :: [PartialFilePath]
               -> PartialFilePath
               -> FileTreeEntity x (LazyMerkleDir m x)
               -> FileTreeEntity x (LazyMerkleDir m x)
               -> ExceptT MergeViolation m (FileTreeEntity x (MergeResult m x))
    onConflict h path (FileEntity x1) (FileEntity x2)
          -- pointers match, they're identical, take either
          | x1 == x2  = pure $ FileEntity x1
          -- file entities are not equal, merge cannot continue
          | otherwise = throwE . MergeViolation $ h ++ [path]

    -- two dirs with the same path
    onConflict h path (DirEntity dir1) (DirEntity dir2)
          -- pointers match, they're identical, take either
          | htPointer dir1 == htPointer dir2 =
              pure $ DirEntity . Fix . Compose $ Left dir1
          | otherwise = DirEntity <$> mergeDirs (h ++ [path]) dir1 dir2

    -- dir replaced with file or vice versa
    onConflict h path _ _ = throwE . MergeViolation $ h ++ [path]


-- TODO: can I fit diff result into this model?
-- type DiffResult m x = Fix (Either (LazyMerkleDir m x) `Compose` ([Diff],) `Compose` Dir x)

type MergeResult m x = Fix (Either (LazyMerkleDir m x) `Compose` Dir x)

data MergeViolation = MergeViolation { mergeViolationPath :: [PartialFilePath] }
  deriving (Eq, Show)
