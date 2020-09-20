{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module HGit.Core.MergeTrie where

--------------------------------------------
import           Data.Aeson as AE
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import           Data.Functor.Compose
import           Data.List.NonEmpty (NonEmpty, toList)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import           Data.Text.Encoding (decodeLatin1, encodeUtf8)
import           Data.Singletons.TH
import           GHC.Generics
--------------------------------------------
import           HGit.Core.Types
import           Merkle.Higher.Types
import           Merkle.Higher.Store
import           Merkle.Higher.Store.Deref
import           Util.RecursionSchemes as R
import           Util.HRecursionSchemes as HR -- YOLO 420 SHINY AND CHROME
--------------------------------------------

-- NOTE: delete operations only valid on files, not directories





data NodeDeleted = NodeDeleted

-- TODO: need to account for unexpanded directory content
data MergeTrie m a
  = MergeTrie
  -- NOTE: is proposed only file hashes? my gut says no, has to be no, to track this merge parent case:
  -- /foo <- file
  -- /foo/bar <- dir with file in it
  -- actually nvm, that would be
  -- MergeTrie { [Hash foo], [], [(foo, MergeTrie { [Hash bar], [], []} )] }
  -- so I guess yes - makes sense, too, file metadata is needed, dir structure is in MergeTrie
  { mtFilesAtPath :: [( LMMT m 'FileTree -- hash of file
                      , LMMT m 'BlobT    -- file blob
                      , LMMT m 'CommitT  -- last modified in this commit
                      , LMMT m 'FileTree    -- previous incarnation
                     )]
  , mtChanges  :: [ChangeType Hash] -- all changes addressed to this node
  , mtChildren :: Map Path ((LMMT m 'FileTree) `Either` a)        -- children with addressed changes
  }
  deriving (Functor, Foldable, Traversable)

-- NOTE FROM RAIN:
-- TWO PASSES, deletes first, then adds, then validates
-- NOTE: it's a set of changes on the trie


-- NOTE: called composite set in mononoke
-- NOTE: basically just multi-way merge
-- Q: how does this evaluate file -> dir or dir -> file merges? idk just represent in DirTree format
-- Q: how does DirTree format keep info needed for reconstruction of full File snapshot
--      specifically - commit last modified in (maybe unchanged for some merge output?)
--                   - last version of file (maybe multiple for multi-way merges)

-- fold a snapshot into a mergetrie
-- buildMergeTrie :: Term (LMM m) 'FileTree -> Fix (MergeTrie m) -> m (Fix (MergeTrie m))
-- buildMergeTrie lmm mt = undefined

-- - compare hashes, return input merge trie if no change

-- empty mergetrie
emptyMergeTrie :: Fix (MergeTrie m)
emptyMergeTrie = Fix $ MergeTrie [] [] Map.empty


-- Plan: first pass is n-way lazy diff. outputs above structure, with diff > initial ChangeType annotations
-- Q: can this represent file>dir and dir>file changes in merge? how to choose which is cannonical?
-- ANS: dir version is cannonical b/c needs recursive structure, file version represented as dir structure with list of add operations
-- ANS: does this imply that it should be _just_ a dir tree annotated with file add operations?
-- ANS: I think yes, it does
-- ANS: this also, I think, means it can be just Compose and not HCompose
--      file is non-recursive so can be just 'Add Hash'


-- Q: how to handle delete -> create -> delete change seq?

-- results in 'm' because it may need to expand the provided tree (which could afterall just be a hash)
applyChange
  :: forall m
   . Monad m
  => Fix (MergeTrie m)
  -> Change Hash
  -> m (Fix (MergeTrie m))
applyChange t c = para f t . toList $ _path c
  where f :: MergeTrie m ( Fix (MergeTrie m)
                         , [Path] -> m (Fix (MergeTrie m))
                         )
          -> [Path]
          -> m (Fix (MergeTrie m))
        f m (path:paths) = case Map.lookup path (mtChildren m) of
          Just (Left lmmt) -> applyChangeH lmmt paths $ _change c
          Just (Right (_,next)) -> next paths
          Nothing -> let mt = MergeTrie
                            { mtFilesAtPath = mtFilesAtPath m
                            , mtChanges = mtChanges m
                            , mtChildren = Map.insert path (Right mt')
                                         $ fmap fst <$> mtChildren m
                            }
                         mt' = constructMT (_change c) paths
                      in pure $ Fix mt
        f m [] = pure . Fix
                      $ MergeTrie { mtFilesAtPath = mtFilesAtPath m
                                  , mtChanges = mtChanges m ++ [_change c]
                                  , mtChildren = fmap fst <$> mtChildren m
                                  }

-- | helper function, constructs merge trie with change at path
constructMT :: forall m. ChangeType Hash -> [Path] -> Fix (MergeTrie m)
constructMT change = R.ana f
  where f :: [Path] -> MergeTrie m [Path]
        f [] = MergeTrie [] [change] Map.empty
        f (x:xs) = MergeTrie [] [] (Map.singleton x (Right xs))

-- | results in 'm' because it may need to expand the provided tree (which could just be a hash)
applyChangeH
  :: forall m
   . Monad m
  => LMMT m 'FileTree
  -> [Path]
  -> ChangeType Hash
  -> m (Fix (MergeTrie m))
applyChangeH lmmt@(Term (HC (Tagged hash (HC (Compose m))))) fullPath ct = do
  m' <- m
  case m' of
    Dir children -> case fullPath of
      [] -> do
        -- port over dir structure
        let children' = fmap Left children
            mt = MergeTrie [] [ct] children'
        pure $ Fix mt

      (path:paths) -> do
        -- recurse into children -or- build new structure at new path
        children' <- case Map.lookup path children of
          Just c -> do
            child <- applyChangeH c paths ct
            pure $ Map.insert path (Right child) $ fmap Left children
          Nothing -> pure $ Map.insert path (Right $ constructMT ct paths) $ fmap Left children
        let mt = MergeTrie [] [] children'
        pure $ Fix mt

    File blob lastMod prev -> case fullPath of
      [] -> do
        let mt = MergeTrie [(lmmt, blob, lastMod, prev)] [ct] Map.empty
        pure $ Fix mt
      (path:paths) -> do
        let children = Map.singleton path . Right $ constructMT ct paths
            mt = MergeTrie [(lmmt, blob, lastMod, prev)] [] children
        pure $ Fix mt
