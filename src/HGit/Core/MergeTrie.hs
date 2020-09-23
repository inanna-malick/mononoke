{-# LANGUAGE LambdaCase #-}
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
import           HGit.Render.Utils
import           Merkle.Higher.Types hiding (Hash)
import           Merkle.Higher.Store
import           Merkle.Higher.Store.Deref
import           Util.RecursionSchemes as R
import           Util.HRecursionSchemes as HR -- YOLO 420 SHINY AND CHROME
--------------------------------------------

-- NOTE: delete operations only valid on files, not directories


-- will add cases to enum
data MergeError = TwoChangesAtPath Path



data NodeDeleted = NodeDeleted

-- TODO: need to account for unexpanded directory content
data MergeTrie m a
  = MergeTrie
  { -- | all files at this path
    mtFilesAtPath :: [(  LMMT m 'FileTree  -- hash of file
                      ,  LMMT m 'BlobT     -- file blob
                      ,  LMMT m 'CommitT   -- last modified in this commit
                      , [LMMT m 'FileTree] -- previous incarnation(s)
                     )]
  -- | all changes at this path
  , mtChanges  :: Maybe (ChangeType (LMMT m)) -- only one change per path is valid
  -- | a map of child entities, if any, each either a recursion
  --   or a pointer to some uncontested extant file tree entity
  , mtChildren :: Map Path ((LMMT m 'FileTree) `Either` a)
  }
  deriving (Functor, Foldable, Traversable)

-- can then use other function to add commit to get snapshot
resolveMergeTrie :: forall m. Hash 'CommitT -> Fix (MergeTrie m) -> WIPT 'FileTree
resolveMergeTrie commit mt = case cata f mt of
      Nothing -> modifiedWIP $ Dir Map.empty
  where
    f :: MergeTrie m (Maybe (WIPT 'FileTree)) -> (Maybe (WIPT 'FileTree))
    f MergeTrie{..} =
      let handleChild (Left (Term (HC (Tagged{..})))) =
            Just $ unmodifiedWIP _tag -- strip out hash from lmmt
          handleChild (Right (Just wipt)) = Just $ wipt
          handleChild (Right Nothing) = Nothing
          children = Map.toList $ Map.mapMaybe handleChild mtChildren
       in case (mtFilesAtPath, mtChanges, children) of
        ([], Nothing, []) -> Nothing -- empty node with no files or changes - delete
        ([], Nothing, _:_) -> -- TODO: more elegant matching statement for 'any nonempty'
          let children' = Map.fromList children      -- no file or change but
           in Just $ modifiedWIP $ Dir children' -- at least one child, retain
        ([], Just Del, _) -> error "delete addressed to a node with no file"
        (fs, Just (Add blob), []) ->
          -- an add addressed to a node with any number of files - simple good state
          Just $ modifiedWIP $ File (unmodifiedWIP $ hashOfLMMT blob)
                                    (unmodifiedWIP commit)
                                    (fmap (\(fh,_,_,_) -> unmodifiedWIP $ hashOfLMMT fh) fs)
        ([], Just (Add _), _:_) -> error "add addressed to node with children"
        ((file, _, _, _):[], Nothing, []) ->
          Just $ unmodifiedWIP $ hashOfLMMT file -- single file with no changes, simple, valid
        (_:_, Just Del, []) -> Nothing -- any number of files, deleted, valid


-- renderMergeTrie :: Fix (MergeTrie m) -> [String]
-- renderMergeTrie = cata f
--   where
--     f :: MergeTrie m [String] -> [String]
--     f MergeTrie{..} = "MergeTrie:" : indent
--                       ( mconcat
--                         [ ["files:"]
--                         , indent $ g <$> mtFilesAtPath
--                         , ["changes:"]
--                         , indent $ h <$> mtChanges
--                         , ["children:"]
--                       ])

-- NOTE FROM RAIN:
-- TWO PASSES, deletes first, then adds, then validates
-- NOTE: it's a set of changes on the trie


-- NOTE: called composite set in mononoke
-- NOTE: basically just multi-way merge
-- Q: how does this evaluate file -> dir or dir -> file merges? idk just represent in DirTree format
-- Q: how does DirTree format keep info needed for reconstruction of full File snapshot
--      specifically - commit last modified in (maybe unchanged for some merge output?)
--                   - last version of file (maybe multiple for multi-way merges)




-- | TODO: function, something like this, that handles actual snapshot creation
makeSnapshot
  :: Monad m
  => LMMT m 'CommitT
  -> (Hash 'CommitT -> m (Maybe (LMMT m 'SnapshotT))) -- index, will be always Nothing for initial WIP
  -> m (M (WIPT) 'SnapshotT)
makeSnapshot commit index = do
  fetchLMMT commit >>= \case
    Commit _ changes parents -> do
      let flip2 = (flip .) . flip
          e = pure ([], emptyMergeTrie)
      (snapshots, mt) <- flip2 foldl e parents $ \mstate parentCommit -> do
        (snapshots, mt) <- mstate
        index (hashOfLMMT parentCommit) >>= \case
          Just snap -> do
            fetchLMMT snap >>= \case
              Snapshot ft _ _ -> do
                mt' <- buildMergeTrie mt ft
                let wipt' = unmodifiedWIP $ hashOfLMMT snap
                    snapshots' = snapshots ++ [wipt']
                pure (snapshots', mt')
          Nothing -> do
            snap@(Snapshot ft _ _) <- makeSnapshot parentCommit index
            mt' <- buildMergeTrie mt ft -- wait shit, ft is expected as LMMT not WIP dead branch FIXME
            let wipt' = modifiedWIP snap
                snapshots' = snapshots ++ [wipt']
            pure (snapshots', mt')

      ft <- resolveMergeTrie (hashOfLMMT commit) <$> applyChanges mt changes
      let commit' = Term $ HC $ L $ hashOfLMMT commit
          snap = Snapshot ft commit' snapshots
      pure snap
    NullCommit -> do
      let ft = modifiedWIP $ Dir Map.empty
          commit' = unmodifiedWIP $ hashOfLMMT commit
          snap = Snapshot ft commit' []
      pure snap

-- | fold a snapshot into a mergetrie
-- TODO: use to write diff, maybe - resulting mergetrie only expands as far as diff boundary
-- TODO: compare hashes
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
                      presentInBoth = zipWithAMatched $ \_ ft e -> case e of
                        Left ft' -> do -- two lmmt 'FileTree nodes, zip if different
                          -- not an elegant solution, but should be valid
                          mt1 <- buildMergeTrie emptyMergeTrie ft
                          mt2 <- buildMergeTrie mt1 ft'
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

-- empty mergetrie
emptyMergeTrie :: Fix (MergeTrie m)
emptyMergeTrie = Fix $ MergeTrie [] Nothing Map.empty


-- Q: how to handle delete -> create -> delete change seq?

applyChanges
  :: forall m
   . Monad m
  => Fix (MergeTrie m)
  -> [Change (LMMT m)] -- could just be 'Change Hash'
  -> m (Fix (MergeTrie m))
applyChanges mt changes = foldl f (pure mt) changes
  where
    f :: m (Fix (MergeTrie m)) -> Change (LMMT m) -> m (Fix (MergeTrie m))
    f mmt c = do mt' <- mmt
                 applyChange mt' c


-- results in 'm' because it may need to expand the provided tree (which could afterall just be a hash)
applyChange
  :: forall m
   . Monad m
  => Fix (MergeTrie m)
  -> Change (LMMT m) -- could just be 'Change Hash'
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
                                  , mtChanges = case mtChanges m of
                                      Nothing -> Just $ _change c
                                      Just x -> error "two changes, TODO proper error"
                                  , mtChildren = fmap fst <$> mtChildren m
                                  }

-- | helper function, constructs merge trie with change at path
constructMT :: forall m. ChangeType (LMMT m) -> [Path] -> Fix (MergeTrie m)
constructMT change = R.ana f
  where f :: [Path] -> MergeTrie m [Path]
        f [] = MergeTrie [] (Just change) Map.empty
        f (x:xs) = MergeTrie [] Nothing (Map.singleton x (Right xs))

-- | results in 'm' because it may need to expand the provided tree (which could just be a hash)
applyChangeH
  :: forall m
   . Monad m
  => LMMT m 'FileTree
  -> [Path]
  -> ChangeType (LMMT m)
  -> m (Fix (MergeTrie m)) -- TODO: ErrorT or something w/ MergeError
applyChangeH lmmt@(Term (HC (Tagged hash (HC (Compose m))))) fullPath ct = do
  m' <- m
  case m' of
    Dir children -> case fullPath of
      [] -> do
        -- port over dir structure
        let children' = fmap Left children
            mt = MergeTrie [] (Just ct) children'
        pure $ Fix mt

      (path:paths) -> do
        -- recurse into children -or- build new structure at new path
        children' <- case Map.lookup path children of
          Just c -> do
            child <- applyChangeH c paths ct
            pure $ Map.insert path (Right child) $ fmap Left children
          Nothing -> pure $ Map.insert path (Right $ constructMT ct paths) $ fmap Left children
        let mt = MergeTrie [] Nothing children'
        pure $ Fix mt

    File blob lastMod prev -> case fullPath of
      [] -> do
        let mt = MergeTrie [(lmmt, blob, lastMod, prev)] (Just ct) Map.empty
        pure $ Fix mt
      (path:paths) -> do
        let children = Map.singleton path . Right $ constructMT ct paths
            mt = MergeTrie [(lmmt, blob, lastMod, prev)] Nothing children
        pure $ Fix mt
