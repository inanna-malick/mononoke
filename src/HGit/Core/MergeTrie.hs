{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module HGit.Core.MergeTrie where

--------------------------------------------
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Data.List.NonEmpty (toList)
import           Data.Maybe (mapMaybe)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Map.Merge.Strict
--------------------------------------------
import           HGit.Core.Types
import           HGit.Render.Utils
import           Util.RecursionSchemes as R
import           Util.HRecursionSchemes as HR -- YOLO 420 SHINY AND CHROME
--------------------------------------------

-- NOTE: delete operations only valid on files, not directories


-- TODO: need to account for unexpanded directory content
data MergeTrie m a
  = MergeTrie
  { -- | all files at this path
    --   using map to enforce only one entry per hash - good idea?
    mtFilesAtPath :: Map (Hash 'FileTree)
                         (  WIPT m 'FileTree  -- hash of file
                         ,  WIPT m 'BlobT     -- file blob
                         ,  WIPT m 'CommitT   -- last modified in this commit
                         , [WIPT m 'FileTree] -- previous incarnation(s)
                         )
  -- | all changes at this path
  , mtChange  :: Maybe (ChangeType (WIPT m)) -- only one change per path is valid (only LMMT-only field)
  -- | a map of child entities, if any, each either a recursion
  --   or a pointer to some uncontested extant file tree entity
  , mtChildren :: Map Path ((WIPT m 'FileTree) `Either` a)
  }
  deriving (Functor, Foldable, Traversable)


-- will add cases to enum
data MergeError
  = MoreThanOneFileButNoChange    -- [Path]
  | DeleteAtNodeWithNoFile        -- [Path]
  | AddChangeAtNodeWithChildren   -- [Path]
  | OneOrMoreFilesButWithChildren -- [Path]
  deriving (Show)

-- can then use other function to add commit to get snapshot
resolveMergeTrie
  :: forall m
   . WIPT m 'CommitT
  -> Fix (MergeTrie m)
  -> MergeError `Either` WIPT m 'FileTree
resolveMergeTrie commit mt = do
    mft <- cata f mt
    case mft of
      Nothing -> pure $ modifiedWIP $ Dir Map.empty
      Just x  -> pure $ x
  where
    f :: MergeTrie m (MergeError `Either` Maybe (WIPT m 'FileTree)) -> MergeError `Either` (Maybe (WIPT m 'FileTree))
    f MergeTrie{..} = do
      children <- Map.toList . Map.mapMaybe id <$> traverse (either (pure . Just) id) mtChildren
      case (snd <$> Map.toList mtFilesAtPath, mtChange, children) of
        ([], Nothing, []) -> pure Nothing -- empty node with no files or changes - delete
        ([], Nothing, _:_) -> -- TODO: more elegant matching statement for 'any nonempty'
          let children' = Map.fromList children                       -- no file or change but
            in pure $ Just $ modifiedWIP $ Dir children' -- at least one child, retain
        ([], Just Del, _) -> Left $ DeleteAtNodeWithNoFile
        (fs, Just (Add blob), []) ->
          -- an add addressed to a node with any number of files - simple good state
          pure $ Just $ modifiedWIP $ File blob
                                            commit
                                            (fmap (\(fh,_,_,_) -> fh) fs)
        ([], Just (Add _), _:_) -> Left $ AddChangeAtNodeWithChildren
        ((file, _, _, _):[], Nothing, []) ->
          pure $ Just file -- single file with no changes, simple, valid
        (_:_, Just Del, []) -> pure Nothing -- any number of files, deleted, valid
        (xs@(x@(f,_,_,_):_:_), Nothing, []) -> -- > 1 file, TODO check if they're the same
          -- if all (\(f',_,_,_) -> hashOfWIPT f' == hashOfWIPT f) xs
          Left $ MoreThanOneFileButNoChange
        (_:_, _, _:_) -> Left $ OneOrMoreFilesButWithChildren


renderMergeTrie :: Fix (MergeTrie m) -> [String]
renderMergeTrie = cata f
  where
    f :: MergeTrie m [String] -> [String]
    f MergeTrie{..} =
      let g :: (Path, ((WIPT m 'FileTree) `Either` [String])) -> [String]
          g (k, (Left wipt)) = [k ++ ": "] ++ renderWIPT wipt
          g (k, (Right v)) = [k ++ ": "] ++ v
          children :: [[String]]
          children = if Map.null mtChildren
            then []
            -- TODO handle either
            else [["children"] ++ (indent $ fmap g $ Map.toList mtChildren)]
          change :: [[String]]
          change = case mtChange of
            Nothing -> []
            Just (Add b) -> [["add"]]
            Just Del -> [["del"]]
          files :: [[String]]
          files = if null mtFilesAtPath
            then []
            else [["files:"] ++ (indent $ pure . showHash <$> Map.keys mtFilesAtPath)]
       in "MergeTrie:" : indent
          ( files ++ change ++ children
          )

-- NOTE FROM RAIN:
-- TWO PASSES, deletes first, then adds, then validates
-- NOTE: it's a set of changes on the trie


-- NOTE: called composite set in mononoke
-- NOTE: basically just multi-way merge
-- Q: how does this evaluate file -> dir or dir -> file merges? idk just represent in DirTree format
-- Q: how does DirTree format keep info needed for reconstruction of full File snapshot
--      specifically - commit last modified in (maybe unchanged for some merge output?)
--                   - last version of file (maybe multiple for multi-way merges)

type IndexRead m  = Hash 'CommitT -> m (Maybe (Hash 'SnapshotT))
type IndexWrite m = Hash 'CommitT -> Hash 'SnapshotT -> m ()

data Index m
  = Index
  { iRead  :: IndexRead m
  , iWrite :: IndexWrite m
  }

stmIOIndex :: MonadIO m => TVar (Map (Hash 'CommitT) (Hash 'SnapshotT)) -> Index m
stmIOIndex tvar
  = let index' = stmIndex tvar
     in Index
  { iRead = \h    -> liftIO $ atomically $ iRead index' h
  , iWrite = \c s -> liftIO $ atomically $ iWrite index' c s
  }


stmIndex :: TVar (Map (Hash 'CommitT) (Hash 'SnapshotT)) -> Index STM
stmIndex tvar
  = Index
  { iRead = \c -> do
      bs <- readTVar tvar
      pure $ Map.lookup c bs
  , iWrite = \c s -> do
      modifyTVar tvar $ \m ->
        Map.insert c s m
      pure ()
  }




nullIndex :: Applicative m => Index m
nullIndex = Index { iRead = \_ -> pure Nothing, iWrite = \_ _ -> pure () }

-- | build a snapshot for a commit, recursively descending
--   into parent commits to build snapshots if neccessary
makeSnapshot
  :: Monad m
  => MonadIO m
  => M (WIPT m) 'CommitT -- can provide LMMT via 'unmodifiedWIP'
  -> IndexRead m -- index, will be always Nothing for initial WIP
  -> StoreRead m -- for expanding index reads
  -> ExceptT MergeError m (M (WIPT m) 'SnapshotT)
makeSnapshot commit index storeRead = do
    (snapshots, mt') <- makeMT commit index storeRead

    -- liftIO $ do
    --   print "mergetrie:"
    --   let lines = renderMergeTrie mt'
    --   traverse (\s -> putStr "  " >> putStrLn s) lines

    ft <- ExceptT $ pure $ resolveMergeTrie (modifiedWIP commit) mt'
    let snap = Snapshot ft (modifiedWIP commit) snapshots

    -- liftIO $ do
    --   print $ "done processing commit: " ++ msg
    --   print "built snapshot with ft:"
    --   let lines = renderWIPT ft
    --   traverse (\s -> putStr "  " >> putStrLn s) lines

    pure snap



-- | build a snapshot for a commit, recursively descending
--   into parent commits to build snapshots if neccessary
makeMT
  :: Monad m
  => MonadIO m
  => M (WIPT m) 'CommitT -- can provide LMMT via 'unmodifiedWIP'
  -> IndexRead m -- index, will be always Nothing for initial WIP
  -> StoreRead m -- for expanding index reads
  -> ExceptT MergeError m ([WIPT m 'SnapshotT], Fix (MergeTrie m))
makeMT commit index storeRead = do
  case commit of
    Commit msg changes parents -> do
      -- lines <- renderLMMT commit
      -- liftIO $ do
      --   print $ "processing commit: " ++ msg
      --   traverse (\s -> putStr "  " >> putStrLn s) lines

      let flip2 = (flip .) . flip
          e = pure ([], emptyMergeTrie)
      (snapshots, mt) <- flip2 foldl e parents $ \mstate parentCommit -> do
        (snapshots, mt) <- mstate
        lift (index (hashOfWIPT parentCommit)) >>= \case
          Just snap -> do
            (HC (Tagged _ snap')) <- lift $ fetchLMMT $ expandHash storeRead snap
            case snap' of
              Snapshot ft _ _ -> do
                mt' <- lift $ buildMergeTrie mt (unmodifiedWIP ft)
                let wipt' = unmodifiedWIP $ expandHash storeRead snap
                    snapshots' = snapshots ++ [wipt']
                pure (snapshots', mt')
          Nothing -> do
            (HC (Tagged _ parentCommit')) <- lift $ fetchWIPT parentCommit
            snap <- makeSnapshot parentCommit' index storeRead
            let (Snapshot ft _ _) = snap
            mt' <- lift $ buildMergeTrie mt ft
            -- liftIO $ do
            --   print "mergetrie:"
            --   let lines = renderMergeTrie mt'
            --   traverse (\s -> putStr "  " >> putStrLn s) lines
            let wipt' = modifiedWIP snap
                snapshots' = snapshots ++ [wipt']
            pure (snapshots', mt')

      mt' <- lift $ applyChanges mt changes

      pure (snapshots, mt')
    NullCommit -> do
      pure ([], emptyMergeTrie)





-- | fold a snapshot into a mergetrie
-- TODO: use to write diff, maybe - resulting mergetrie only expands as far as diff boundary
-- TODO: compare hashes, short circuit - not yet impl'd, needed to get example working
buildMergeTrie :: forall m. Monad m => Fix (MergeTrie m) -> WIPT m 'FileTree -> m (Fix (MergeTrie m))
buildMergeTrie mt = para f mt
  where f :: MergeTrie m ( Fix (MergeTrie m)
                         , WIPT m 'FileTree -> m (Fix (MergeTrie m))
                         )
          -> WIPT m 'FileTree
          -> m (Fix (MergeTrie m))
        f mt wipt = do
              HC (Tagged hash m') <- fetchWIPT wipt
              case m' of
                Dir children -> do
                  let wiptOnly = traverseMissing $ \_ ft -> pure $ Left ft
                      presentInBoth = zipWithAMatched $ \_ ft e -> case e of
                        Left ft' ->
                           -- two WIPT 'FileTree nodes, zip if different
                          if (hashOfWIPT ft == hashOfWIPT ft')
                            then do -- short circuit if hash (==)
                              pure $ Left ft'
                            else do
                              -- not an elegant solution, but should be valid (FIXME/TODO: ???, revisit)
                              mt1 <- buildMergeTrie emptyMergeTrie ft
                              mt2 <- buildMergeTrie mt1 ft'
                              pure $ Right mt2

                        Right (_,next) -> Right <$> next ft
                      mtOnly = traverseMissing $ \_ e -> pure $ fmap fst e

                  mtChildren'
                    <- mergeA wiptOnly mtOnly presentInBoth children (mtChildren mt)

                  let mt' = MergeTrie
                          { mtFilesAtPath = mtFilesAtPath mt
                          , mtChange = mtChange mt
                          , mtChildren = mtChildren'
                          }

                  pure $ Fix mt'
                File blob lastMod prev -> do
                  let mt' = MergeTrie
                          { mtFilesAtPath = Map.insert
                              (hashOfWIPT wipt)
                              (wipt, blob, lastMod, prev)
                              (mtFilesAtPath mt)
                          , mtChange = mtChange mt
                          , mtChildren = fmap fst <$> mtChildren mt
                          }
                  pure $ Fix mt'

-- empty mergetrie
emptyMergeTrie :: Fix (MergeTrie m)
emptyMergeTrie = Fix $ MergeTrie Map.empty Nothing Map.empty


-- Q: how to handle delete -> create -> delete change seq?

applyChanges
  :: forall m
   . Monad m
  => Fix (MergeTrie m)
  -> [Change (WIPT m)] -- could just be 'Change Hash'
  -> m (Fix (MergeTrie m))
applyChanges mt changes = foldl f (pure mt) changes
  where
    f :: m (Fix (MergeTrie m)) -> Change (WIPT m) -> m (Fix (MergeTrie m))
    f mmt c = do mt' <- mmt
                 applyChange mt' c


-- results in 'm' because it may need to expand the provided tree (which could afterall just be a hash)
applyChange
  :: forall m
   . Monad m
  => Fix (MergeTrie m)
  -> Change (WIPT m) -- could just be 'Change Hash'
  -> m (Fix (MergeTrie m))
applyChange t c = para f t . toList $ _path c
  where f :: MergeTrie m ( Fix (MergeTrie m)
                         , [Path] -> m (Fix (MergeTrie m))
                         )
          -> [Path]
          -> m (Fix (MergeTrie m))
        f m (path:paths) = do
          mt' <- case Map.lookup path (mtChildren m) of
                  Just (Left lmmt) -> applyChangeH lmmt paths $ _change c
                  Just (Right (_,next)) -> next paths
                  Nothing -> pure $ constructMT (_change c) paths
          pure $ Fix $ MergeTrie
                     { mtFilesAtPath = mtFilesAtPath m
                     , mtChange      = mtChange m
                     , mtChildren    = Map.insert path (Right mt')
                                     $ fmap fst <$> mtChildren m
                     }
        f m [] = pure . Fix
                      $ MergeTrie { mtFilesAtPath = mtFilesAtPath m
                                  , mtChange = case mtChange m of
                                      Nothing -> Just $ _change c
                                      Just _x -> -- FIXME: maybe do something else in this case, eg error (needs error channel)
                                        Just $ _change c -- overwrite previous change at path
                                  , mtChildren = fmap fst <$> mtChildren m
                                  }

-- | helper function, constructs merge trie with change at path
constructMT :: forall m. ChangeType (WIPT m) -> [Path] -> Fix (MergeTrie m)
constructMT change = R.ana f
  where f :: [Path] -> MergeTrie m [Path]
        f [] = MergeTrie Map.empty (Just change) Map.empty
        f (x:xs) = MergeTrie Map.empty Nothing (Map.singleton x (Right xs))

-- | results in 'm' because it may need to expand the provided tree (which could just be a hash)
applyChangeH
  :: forall m
   . Monad m
  => WIPT m 'FileTree
  -> [Path]
  -> ChangeType (WIPT m)
  -> m (Fix (MergeTrie m)) -- TODO: ErrorT or something w/ MergeError
applyChangeH wipt fullPath ct = do
  HC (Tagged hash m') <- fetchWIPT wipt
  case m' of
    Dir children -> case fullPath of
      [] -> do
        -- port over dir structure
        let children' = fmap Left children
            mt = MergeTrie Map.empty (Just ct) children'
        pure $ Fix mt

      (path:paths) -> do
        -- recurse into children -or- build new structure at new path
        children' <- case Map.lookup path children of
          Just c -> do
            child <- applyChangeH c paths ct
            pure $ Map.insert path (Right child) $ fmap Left children
          Nothing -> pure $ Map.insert path (Right $ constructMT ct paths) $ fmap Left children
        let mt = MergeTrie Map.empty Nothing children'
        pure $ Fix mt

    File blob lastMod prev -> case fullPath of
      [] -> do
        let files = Map.singleton (hashOfWIPT wipt) (wipt, blob, lastMod, prev)
            mt = MergeTrie files (Just ct) Map.empty
        pure $ Fix mt
      (path:paths) -> do
        let children = Map.singleton path . Right $ constructMT ct paths
            files = Map.singleton (hashOfWIPT wipt) (wipt, blob, lastMod, prev)
            mt = MergeTrie files Nothing children
        pure $ Fix mt
