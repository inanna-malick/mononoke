{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module HGit.Core.MergeTrie where

--------------------------------------------
import           Control.Concurrent.STM
import           Control.Monad.Trans
import           Control.Monad.Except
import qualified Data.Foldable as Foldable
import           Data.Functor.Compose
import           Data.List.NonEmpty (NonEmpty, toList, nonEmpty)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Map.Merge.Strict
--------------------------------------------
import           HGit.Core.Types
import           Util.RecursionSchemes as R
import           Util.HRecursionSchemes
--------------------------------------------

-- | represents assertions from N snapshots
data SnapshotTrie m a
  = SnapshotTrie
  { -- | all files at this path
    --   using map to enforce only one entry per hash - good idea?
    stFilesAtPath :: Map (Hash 'FileTree)
                         (WIPT m 'FileTree, SnapshotFile (WIPT m))
  -- | a map of child entities, if any, each either a recursion
  --   or a pointer to some uncontested extant file tree entity
  , stChildren :: Map Path ((WIPT m 'FileTree) `Either` a)
  }
  deriving (Functor, Foldable, Traversable)

-- | represents assertions from N snapshots and a commit
--   used to resolve merges
data MergeTrie m a
  = MergeTrie
  { -- | Trie layer containing all assertions from snapshots
    mtSnapshotTrie :: SnapshotTrie m a
    -- | all changes at this path
  , mtChange  :: Maybe (ChangeType (WIPT m)) -- only one change per path is valid (only LMMT-only field)
  }
  deriving (Functor, Foldable, Traversable)

-- NOTE/FIXME: this needs to operate on git-style trees - can I parameterize the whole trie infra over the underlying file type (eg Mononoke FT w/ commit pointers vs git-style)
-- | represents assertions from N parent snapshots and child snapshot
--   used to generate change lists from git-style trees
-- data MergeTrie m a
--   = MergeTrie
--   { -- | Trie layer containing all assertions from snapshots
--     mtSnapshotTrie :: SnapshotTrie m a
--     -- | all changes at this path
--   , mtChange  :: Maybe (ChangeType (WIPT m)) -- only one change per path is valid (only LMMT-only field)
--   }
--   deriving (Functor, Foldable, Traversable)



mtChildren
  :: MergeTrie m a
  -> Map Path ((WIPT m 'FileTree) `Either` a)
mtChildren = stChildren . mtSnapshotTrie

mtFilesAtPath
  :: MergeTrie m a
  -> Map (Hash 'FileTree) (WIPT m 'FileTree, SnapshotFile (WIPT m))
mtFilesAtPath = stFilesAtPath . mtSnapshotTrie

-- will add cases to enum
data MergeErrorAtPath
  = MoreThanOneFileButNoChange
  | DeleteAtNodeWithNoFile
  | AddChangeAtNodeWithChildren
  | OneOrMoreFilesButWithChildren
  deriving (Show)

data MergeError
  = ErrorAtPath [Path] MergeErrorAtPath
  | InvalidChange ApplyChangeError
  deriving (Show)

-- single layer of error annotated merge trie
type ErrorAnnotatedMergeTrie m = Either (Fix (MergeTrie m)) -- potentially a subtrie with no errors
                       `Compose` (,) (Maybe MergeErrorAtPath) -- each node potentially error tagged
                       `Compose` MergeTrie m -- the actual merge trie structure

-- TODO move to some utils module
type RAlgebra f a = f (Fix f, a) -> a


-- can then use other function to add commit to get snapshot
resolveMergeTrie
  :: forall m
   . WIPT m 'CommitT
  -> Fix (MergeTrie m)
  -> (NonEmpty MergeError) `Either` WIPT m 'FileTree
resolveMergeTrie c mt = either (\e -> Left $ convertErrs $ cata f e []) Right x
  where
    x = resolveMergeTrie' c mt
    convertErrs :: [MergeError] -> (NonEmpty MergeError)
    convertErrs errs = case nonEmpty errs of
      Just xs -> xs
      Nothing ->
        error "algorithm invariant broken: must be at least one error in ErrorAnnotatedMergeTrie"

    f :: Algebra (ErrorAnnotatedMergeTrie m) ([Path] -> [MergeError])
    f (Compose (Left _)) _ = []
    f (Compose (Right (Compose (me, mt'@MergeTrie{..})))) path =
      let me' = maybe [] (\e -> [ErrorAtPath path e]) me
       in Foldable.fold (g path <$> Map.toList (mtChildren mt')) ++ me'

    g :: forall x
       . [Path]
      -> (Path, Either x ([Path] -> [MergeError]))
      -> [MergeError]
    g path (pathSegment, enext) = either (const []) id $ (\h -> h $ path ++ [pathSegment]) <$> enext



-- can then use other function to add commit to get snapshot
resolveMergeTrie'
  :: forall m
   . WIPT m 'CommitT
  -> Fix (MergeTrie m)
  -> Fix (ErrorAnnotatedMergeTrie m) `Either` WIPT m 'FileTree
resolveMergeTrie' commit root = do
    mft <- para g root
    case mft of
      Nothing -> pure $ modifiedWIP $ Dir Map.empty
      Just x  -> pure $ x
  where
    g :: RAlgebra (MergeTrie m) (Fix (ErrorAnnotatedMergeTrie m) `Either` Maybe (WIPT m 'FileTree))
    g mt@MergeTrie{..} = do

      let liftMT :: ( Fix (MergeTrie m)
                    , Fix (ErrorAnnotatedMergeTrie m) `Either` Maybe (WIPT m 'FileTree)
                    )
                 -> Fix (ErrorAnnotatedMergeTrie m)
          -- this assumes no nested further errors, I think I need to recurse here, or at least look at the snd arg of the input
          liftMT = Fix . Compose . Left . fst
          liftErr' :: Maybe MergeErrorAtPath -> Fix (ErrorAnnotatedMergeTrie m) `Either` Maybe (WIPT m 'FileTree)
          liftErr' me = Left $ Fix $ Compose $ Right $ Compose (me, fmap (liftMT) mt)
          liftErr = liftErr' . Just


      let echildren = Map.toList . Map.mapMaybe id <$> traverse (either (pure . Just) snd) (mtChildren mt)
          echildren' = case echildren of
            Right x -> Right x
            -- if we hit an error in a child node, we can't just return that error annotated subtree
            -- so reconstruct the current tree with all children lifted to error annotated subtrees

            -- NOTE: UI tests show only most-nested error being displayed
            Left  _ ->
              let f :: ( Fix (MergeTrie m)
                       , Fix (ErrorAnnotatedMergeTrie m) `Either` Maybe (WIPT m 'FileTree)
                       )
                    -> Fix (ErrorAnnotatedMergeTrie m)
                  f (t,e) = either id (const $ liftMT (t,e)) e
                  -- aha, Nothing - this is preventing errors from being seen if a nested child has an error
                  -- but how could this be fixed? do I really want to run that case match block below if children are errored out?
                  -- FIXME/TODO ^^ figure this out
               in Left $ Fix $ Compose $ Right $ Compose (Nothing, fmap f mt)

      children <- echildren'

      case ( fst . snd <$> Map.toList (mtFilesAtPath mt) -- files at path       (candidates from different merge commits)
           , mtChange                         -- optional change     (from currently applied commit)
           , children                         -- child nodes at path (child nodes)
           ) of
        ([], Nothing, []) -> pure Nothing -- empty node with no files or changes - delete
        ([], Nothing, _:_) -> -- TODO: more elegant matching statement for 'any nonempty'
          let children' = Map.fromList children          -- no file or change but
            in pure $ Just $ modifiedWIP $ Dir children' -- at least one child, retain
        ([], Just Del, _) -> liftErr DeleteAtNodeWithNoFile
        (fs, Just (Add blob), []) ->
          -- an add addressed to a node with any number of files - simple good state
          pure $ Just $ modifiedWIP $ File $ SnapshotFile blob
                                                          commit
                                                         (fmap (\fh -> fh) fs)
        ([], Just (Add _), _:_) -> liftErr AddChangeAtNodeWithChildren
        ([file], Nothing, []) ->
          pure $ Just file -- single file with no changes, simple, valid
        (_:_, Just Del, []) -> pure Nothing -- any number of files, deleted, valid
        ((_:_:_), Nothing, []) -> -- > 1 file at same path, each w/ different hashes (b/c list converted from map)
          liftErr MoreThanOneFileButNoChange
        (_:_, Nothing, _:_)        -> liftErr OneOrMoreFilesButWithChildren
        (_:_, Just (Add _), _:_)   -> liftErr OneOrMoreFilesButWithChildren
        -- one or more files, but with children, but files at this path are deleted so it's valid
        (_:_, Just Del, _:_)       ->
          let children' = Map.fromList children
            in pure $ Just $ modifiedWIP $ Dir children'

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
  -> ExceptT (NonEmpty MergeError) m (M (WIPT m) 'SnapshotT)
makeSnapshot commit index storeRead = do
    (snapshots, mt') <- case commit of
        Commit _msg changes parents -> makeMT changes parents index storeRead
        NullCommit -> pure ([], emptyMergeTrie)

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
  => [Change (WIPT m)]          -- list of inline changes
  -> NonEmpty (WIPT m 'CommitT) -- parent commits
  -> IndexRead m -- index, will be always Nothing for initial WIP
  -> StoreRead m -- for expanding index reads
  -> ExceptT (NonEmpty MergeError) m ([WIPT m 'SnapshotT], Fix (MergeTrie m))
makeMT changes parents index storeRead = do
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

      mt' <- ExceptT $ fmap (either (Left . pure . InvalidChange) Right) $ runExceptT $ applyChanges mt changes

      pure (snapshots, mt')


-- | fold a snapshot into a mergetrie
buildMergeTrie :: forall m. Monad m => Fix (MergeTrie m) -> WIPT m 'FileTree -> m (Fix (MergeTrie m))
buildMergeTrie original = para f original
  where f :: MergeTrie m ( Fix (MergeTrie m)
                         , WIPT m 'FileTree -> m (Fix (MergeTrie m))
                         )
          -> WIPT m 'FileTree
          -> m (Fix (MergeTrie m))
        f mt wipt = do
              HC (Tagged _hash m') <- fetchWIPT wipt
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
                          { mtChange = mtChange mt
                          , mtSnapshotTrie = SnapshotTrie
                                           { stChildren = mtChildren'
                                           , stFilesAtPath = mtFilesAtPath mt
                                           }
                          }

                  pure $ Fix mt'
                File sf -> do
                  let filesAtPath = Map.insert (hashOfWIPT wipt) (wipt, sf) (mtFilesAtPath mt)
                      mt' = MergeTrie
                          { mtChange = mtChange mt
                          , mtSnapshotTrie = SnapshotTrie
                                           { stChildren    = fmap fst <$> mtChildren mt
                                           , stFilesAtPath = filesAtPath
                                           }
                          }
                  pure $ Fix mt'

-- empty mergetrie
emptyMergeTrie :: Fix (MergeTrie m)
emptyMergeTrie = Fix
               $ MergeTrie
               { mtChange = Nothing
               , mtSnapshotTrie = SnapshotTrie
                               { stChildren = Map.empty
                               , stFilesAtPath = Map.empty
                               }
               }



data ApplyChangeError
  = ChangeAlreadyExistsAtPath
  deriving (Show)

-- TODO: output error-annotated list of commits? eh, maybe later, not a priority
applyChanges
  :: forall m
   . Monad m
  => Fix (MergeTrie m)
  -> [Change (WIPT m)] -- could just be 'Change Hash'
  -> ExceptT ApplyChangeError m (Fix (MergeTrie m))
applyChanges mt changes = foldl f (pure mt) changes
  where
    f :: ExceptT ApplyChangeError m (Fix (MergeTrie m)) -> Change (WIPT m) -> ExceptT ApplyChangeError m (Fix (MergeTrie m))
    f mmt c = do mt' <- mmt
                 applyChange mt' c


-- results in 'm' because it may need to expand the provided tree (which could afterall just be a hash)
applyChange
  :: forall m
   . Monad m
  => Fix (MergeTrie m)
  -> Change (WIPT m) -- could just be 'Change Hash'
  -> ExceptT ApplyChangeError m (Fix (MergeTrie m))
applyChange t c = para f t . toList $ _path c
  where f :: MergeTrie m ( Fix (MergeTrie m)
                         , [Path] -> ExceptT ApplyChangeError m (Fix (MergeTrie m))
                         )
          -> [Path]
          -> ExceptT ApplyChangeError m (Fix (MergeTrie m))
        f m (path:paths) = do
          mt' <- case Map.lookup path (mtChildren m) of
                  Just (Left lmmt) -> lift $ applyChangeH lmmt paths $ _change c
                  Just (Right (_,next)) -> next paths
                  Nothing -> pure $ constructMT (_change c) paths
          pure $ Fix
               $ MergeTrie
               { mtChange = mtChange m
               , mtSnapshotTrie = SnapshotTrie
                               { stChildren = Map.insert path (Right mt')
                                            $ fmap fst <$> mtChildren m

                               , stFilesAtPath = mtFilesAtPath m
                               }
               }
        f m [] = do
          change' <- case mtChange m of
            Nothing -> pure $ Just $ _change c
            Just _  -> ExceptT $ pure $ Left $ ChangeAlreadyExistsAtPath
          pure . Fix
               $ MergeTrie
               { mtChange = change'
               , mtSnapshotTrie = SnapshotTrie
                               { stChildren = fmap fst <$> mtChildren m
                               , stFilesAtPath = mtFilesAtPath m
                               }
               }

-- | helper function, constructs merge trie with change at path
constructMT :: forall m. ChangeType (WIPT m) -> [Path] -> Fix (MergeTrie m)
constructMT change = R.ana f
  where f :: [Path] -> MergeTrie m [Path]
        f []     = MergeTrie
                 { mtChange = Just change
                 , mtSnapshotTrie = SnapshotTrie
                                  { stChildren = Map.empty
                                  , stFilesAtPath = Map.empty
                                  }
                 }
        f (x:xs) = MergeTrie
                 { mtChange = Nothing
                 , mtSnapshotTrie = SnapshotTrie
                                  { stChildren = Map.singleton x (Right xs)
                                  , stFilesAtPath = Map.empty
                                  }
                 }


-- | results in 'm' because it may need to expand the provided tree (which could just be a hash)
applyChangeH
  :: forall m
   . Monad m
  => WIPT m 'FileTree
  -> [Path]
  -> ChangeType (WIPT m)
  -> m (Fix (MergeTrie m))
applyChangeH wipt fullPath ct = do
  HC (Tagged _hash m') <- fetchWIPT wipt
  case m' of
    Dir children -> case fullPath of
      [] -> do
        -- port over dir structure
        let children' = fmap Left children
            mt = MergeTrie
               { mtChange = Just ct
               , mtSnapshotTrie = SnapshotTrie
                                { stChildren = children'
                                , stFilesAtPath = Map.empty
                                }
               }

        pure $ Fix mt

      (path:paths) -> do
        -- recurse into children -or- build new structure at new path
        children' <- case Map.lookup path children of
          Just c -> do
            child <- applyChangeH c paths ct
            pure $ Map.insert path (Right child) $ fmap Left children
          Nothing -> pure $ Map.insert path (Right $ constructMT ct paths) $ fmap Left children
        let mt = MergeTrie
               { mtChange = Nothing
               , mtSnapshotTrie = SnapshotTrie
                                { stChildren = children'
                                , stFilesAtPath = Map.empty
                                }
               }

        pure $ Fix mt

    File sf -> case fullPath of
      [] -> do
        let files = Map.singleton (hashOfWIPT wipt) (wipt, sf)
            mt = MergeTrie
               { mtChange = Just ct
               , mtSnapshotTrie = SnapshotTrie
                                { stChildren = Map.empty
                                , stFilesAtPath = files
                                }
               }
        pure $ Fix mt
      (path:paths) -> do
        let children = Map.singleton path . Right $ constructMT ct paths
            files = Map.singleton (hashOfWIPT wipt) (wipt, sf)
            mt = MergeTrie
               { mtChange = Nothing
               , mtSnapshotTrie = SnapshotTrie
                                { stChildren = children
                                , stFilesAtPath = files
                                }
               }
        pure $ Fix mt
