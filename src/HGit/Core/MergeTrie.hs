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
import           Data.Map.Merge.Strict
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
  { -- | all files at this path
    mtFilesAtPath :: [( LMMT m 'FileTree -- hash of file
                      , LMMT m 'BlobT    -- file blob
                      , LMMT m 'CommitT  -- last modified in this commit
                      , LMMT m 'FileTree -- previous incarnation
                     )]
  -- | all changes at this path
  , mtChanges  :: [ChangeType Hash]
  -- | a map of child entities, if any, each either a recursion
  --   or a pointer to some uncontested extant file tree entity
  , mtChildren :: Map Path ((LMMT m 'FileTree) `Either` a)
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

-- | fold a snapshot into a mergetrie
-- TODO: use to write diff, maybe - resulting mergetrie only expands as far as diff boundary
buildMergeTrie :: forall m. Monad m => Fix (MergeTrie m) -> LMMT m 'FileTree -> m (Fix (MergeTrie m))
buildMergeTrie mt = para f mt
  where f :: MergeTrie m ( Fix (MergeTrie m)
                         , LMMT m 'FileTree -> m (Fix (MergeTrie m))
                         )
          -> LMMT m 'FileTree
          -> m (Fix (MergeTrie m))
        f mt lmmt@(Term (HC (Tagged hash (HC (Compose m))))) = do
              m' <- m
              case m' of
                Dir children -> do
                  let lmmtOnly = traverseMissing $ \_ ft -> pure $ Left ft
                      -- presentInBoth :: WhenMatched m Path
                      --   ( (LMMT m 'FileTree)
                      --     `Either` ( Fix (MergeTrie m)
                      --              , LMMT m 'FileTree -> m (Fix (MergeTrie m))
                      --              )
                      --   )
                      --   (LMMT m 'FileTree)
                      --   ((LMMT m 'FileTree) `Either` Fix (MergeTrie m))
                      presentInBoth = zipWithAMatched $ \_ ft e -> case e of
                        Left ft' -> do -- two lmmt 'FileTree nodes, zip if different
                          -- not an elegant solution, but should be valid
                          mt1 <- buildMergeTrie emptyMergeTrie ft
                          mt2 <- buildMergeTrie emptyMergeTrie ft'
                          pure $ Right mt2
                        -- ^^ NOTE: hey I was right, this is a superset of `diff`
                        Right (_,next) -> Right <$> next ft
                      mtOnly = traverseMissing $ \_ e -> pure $ fmap fst e

                  mtChildren'
                    <- mergeA lmmtOnly mtOnly presentInBoth children (mtChildren mt)

                  let mt' = MergeTrie
                          { mtFilesAtPath = mtFilesAtPath mt
                          , mtChanges = mtChanges mt
                          , mtChildren = mtChildren'
                          }

                  pure $ Fix mt'
                File blob lastMod prev -> do
                  let mt' = MergeTrie
                          { mtFilesAtPath = mtFilesAtPath mt ++ [(lmmt, blob, lastMod, prev)]
                          , mtChanges = mtChanges mt
                          , mtChildren = fmap fst <$> mtChildren mt
                          }
                  pure $ Fix mt'


-- - compare hashes, return input merge trie if no change

-- empty mergetrie
emptyMergeTrie :: Fix (MergeTrie m)
emptyMergeTrie = Fix $ MergeTrie [] [] Map.empty


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
