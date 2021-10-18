{-# LANGUAGE TypeFamilies #-}
{-# language ScopedTypeVariables        #-}



module Merkle.App where


import           Merkle.Bonsai.Types
import           Merkle.Bonsai.MergeTrie
import           Merkle.Generic.HRecursionSchemes
import           Merkle.Generic.Merkle as M
import           Merkle.Generic.DAGStore (mkGRPCClient, mkClient)

import           System.Directory
import qualified Data.Map.Merge.Strict as M
import qualified Data.Map.Strict as M
import           Control.Monad.Except
import           Data.List.NonEmpty (NonEmpty)
import           Data.Aeson as AE
import GHC.Generics



empty :: Applicative m => LMMT m 'FileTree
empty = liftLMMT $ Term $ Dir M.empty



-- commands to implement:
-- mkbranch: create branch
-- delbranch: delete branch
-- checkout: impose remote state on local subdir (note to self: use containers to test this)
-- commit: commit all state in repo with message
-- merge: list of commits, merge all into current commit, simple diff resolution (?) or fail
--  - OR: enter merge in progress state, 'current commit' is instead NEL of parents, resolution pending?

-- diff: show extant changes (basically just commit dry run, super easy kinda )


-- NOTE: no need for snapshot browser/blame/etc, that's all via the web UI

-- status: show current branch, also list modified files (stretch goal, can just cat json state)

-- branch off current commit
mkBranch :: String -> LocalState -> Either String LocalState
mkBranch name ls = case M.member name (branches ls) of
  False -> Right $ ls { branches = M.insert name (currentCommit ls) (branches ls) }
  True  -> Left $ "mk branch that already exists " ++ name

-- delete existing branch
delBranch :: String -> LocalState -> Either String LocalState
delBranch name ls = case M.member name (branches ls) of
  False -> Right $ ls { branches = M.delete name $ branches ls }
  True  -> Left $ "del branch " ++ name ++ " that doesn't exist"



-- can just require cmd line to be in root dir of repo, not in a subdir
-- state store: local staged, as json, also presence of a file signifies that it's a mononoke root
data LocalState
  = LocalState
  { backingStoreAddr   :: String
  , backingStorePort   :: Integer
  , currentCommit      :: Hash 'CommitT
  , currentBranch      :: Maybe String
  , snapshotMappings   :: M.Map (Hash 'CommitT) (Hash 'SnapshotT)
  , branches           :: M.Map String (Hash 'CommitT)
  } deriving (Ord, Eq, Show, Generic)


instance ToJSON LocalState where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON LocalState



initialLocalState
  :: forall m
   . ( MonadIO m
     , MonadError String m
     )
  => String -- store addr
  -> Integer -- store port
  -> m LocalState
initialLocalState addr port = do
  let clientConfig = mkGRPCClient addr (fromInteger port)
  client <- mkClient clientConfig
  let store = mkDagStore client
  emptyCommit <- sWrite store $ NullCommit
  pure $ LocalState
       { backingStoreAddr   = addr
       , backingStorePort   = port
       , snapshotMappings   = M.empty
       , branches           = M.empty
       , currentCommit      = emptyCommit
       , currentBranch      = Just "main"
       }


-- set up dir with initial state
init
  :: forall m
   . ( MonadError String m
     , MonadIO m
     )
  => NonEmpty Path
  -> m ()
init path = do
  let path' = concatPath $ path <> pure localStateName
  -- check if exists
  isFile <- liftIO $ doesFileExist path'
  case isFile of
    True -> do
      throwError $ "state file already exists at " ++ path'
    False ->
      initialLocalState "localhost" 8080 >>= writeLocalState path
  pure ()


localStateName :: Path
localStateName = ".bonsai.state"



readLocalState
  :: forall m
   . ( MonadError String m
     , MonadIO m
     )
  => NonEmpty Path
  -> m LocalState
readLocalState containingDir = do
  let path = concatPath (containingDir <> pure localStateName)
  liftIO (eitherDecodeFileStrict path) >>= liftEither


writeLocalState
  :: forall m
   . ( MonadIO m
     )
  => NonEmpty Path
  -> LocalState
  -> m ()
writeLocalState containingDir ls = do
  let path = concatPath (containingDir <> pure localStateName)
  liftIO $ encodeFile path ls


-- PLAN: app that runs in dir w/ app root, hits remote DAG store server
-- PLAN: same binary for both

commit
  :: forall m
   . ( MonadError String m
     , MonadIO m
     )
  => NonEmpty Path
  -> String
  -> m [Change (Term M)]
commit root commitMsg = do
  localState <- readLocalState root
  let clientConfig = mkGRPCClient (backingStoreAddr localState) (fromInteger $ backingStorePort localState)
  client <- mkClient clientConfig
  let store = mkDagStore client
  snapshot <- case M.lookup (currentCommit localState) (snapshotMappings localState) of
    Nothing -> do
      lastCommit <- lmLazy $ unTerm $ lazyLoadHash store (currentCommit localState)
      let lastCommit' :: M (WIPT m) 'CommitT
            = hfmap (unmodifiedWIP . toLMT) lastCommit
      snapshotEWIP <- runExceptT $ makeSnapshot lastCommit' (iRead nullIndex) (sRead store)
      snapshotWIP <- either (throwError . ("merge errors in history during commit op" ++) . show) pure snapshotEWIP
      snapshot <- uploadWIPT (sWrite store) $ modifiedWIP snapshotWIP
      pure $ snapshot
    Just snapshotHash -> do
      pure $ toLMT $ lazyLoadHash store snapshotHash
  (HC (Tagged _ snapshot')) <- fetchLMMT snapshot
  let (Snapshot ft _ _) = snapshot'
  diffs <- diffLocalState root ft
  wipCommit <- case diffs of
    [] -> throwError "attempted commit with no diffs"
    changes -> do
      let parent = toLMT $ lazyLoadHash store (currentCommit localState)
      let changes' = mapChange modifiedWIP' <$> changes
      pure $ modifiedWIP $ Commit commitMsg changes' (pure $ unmodifiedWIP parent)
  newCommitHash <- hashOfLMMT <$> uploadWIPT (sWrite store) wipCommit

  let localState' = localState { currentCommit =  newCommitHash}
  writeLocalState root localState'

  -- NOTE: doesn't establish snapshot for new commit - should do so to confirm validity LMAO TODO

  pure diffs


-- working, could do with some polish and tests (lmao) and etc
diffLocalState
  :: forall m
   . ( MonadError String m
     , MonadIO m
     )
  => NonEmpty Path
  -> LMMT m 'FileTree
  -> m [Change (Term M)]
diffLocalState root snapshot = processRoot snapshot
  where
    listDirectory' x = filter (/= localStateName) <$> listDirectory x
    processRoot :: LMMT m 'FileTree -> m [Change (Term M)]
    processRoot lmmt = do
      liftIO $ putStrLn "processRoot"
      contents <- liftIO $ listDirectory' $ concatPath root
      let local = M.fromList $ fmap (\a -> (a, ())) contents
      remote <- do
        (HC (Tagged _ m)) <- fetchLMMT lmmt
        case m of
          File _ -> throwError "expected root path to be a dir in LMMT"
          Dir  d -> pure d
      mconcat . fmap snd . M.toList <$>
        M.mergeA (M.traverseMissing $ remoteMissing' . pure)
                 (M.traverseMissing $ localMissing   . pure)
                 (M.zipWithAMatched $ bothPresent    . pure)
                  local remote


    -- both paths present, file/dir conflict still possible
    bothPresent :: NonEmpty Path -> () -> LMMT m 'FileTree -> m [Change (Term M)]
    bothPresent path () lmmt = do
      liftIO $ putStrLn "bothPresent"
      let absolutePath = concatPath $ root <> path
      (HC (Tagged _ m)) <- fetchLMMT lmmt
      case m of
        Dir  remoteDirContents  -> do
          isDir <- liftIO $ doesDirectoryExist absolutePath
          case isDir of
            False -> do -- remote dir, local ???, if file then add/remote del, else ???
              isFile <- liftIO $ doesFileExist absolutePath
              case isFile of
                False -> throwError $ "Dir replaced with non-file type entity at " ++ show path
                True  -> do -- remote dir, local file
                  remoteDeletes <- traverse (uncurry $ localMissing . (path <>) . pure)
                             $ M.toList remoteDirContents
                  localContents <- liftIO $ readFile absolutePath
                  pure $ [Change path $ Add $ Term $ Blob localContents] ++ mconcat remoteDeletes

            True  -> do
              contents <- liftIO $ listDirectory' absolutePath
              let local = M.fromList $ fmap (\a -> (a, ())) contents
              mconcat . fmap snd . M.toList <$>
                M.mergeA (M.traverseMissing $ remoteMissing' . (path <>) . pure)
                         (M.traverseMissing $ localMissing   . (path <>) . pure)
                         (M.zipWithAMatched $ bothPresent    . (path <>) . pure)
                          local remoteDirContents

        File remoteContentsLMMT -> do
          isFile <- liftIO $ doesFileExist absolutePath
          case isFile of
            True -> do -- diff contents. potential optimization, hash local before blob fetch.
              (HC (Tagged _ remoteBlob)) <- fetchLMMT $ sfBlob remoteContentsLMMT
              let (Blob remoteContents) = remoteBlob
              localContents <- liftIO $ readFile absolutePath
              case localContents == remoteContents of
                True -> pure [] -- no change
                False -> pure [Change path $ Add $ Term $ Blob localContents]
            False -> do
              localChanges <- do
                isDir <- liftIO $ doesDirectoryExist absolutePath
                case isDir of
                  False -> pure [] -- not a dir or a file, shrug emoji
                  True  -> do
                    contents <- liftIO $ listDirectory' absolutePath
                    mconcat <$> traverse (remoteMissing . (path <>) . pure) contents
              pure $ [Change path Del] ++ localChanges



    localMissing :: NonEmpty Path -> LMMT m 'FileTree -> m [Change (Term M)]
    localMissing path lmmt = do
      liftIO $ putStrLn "localMissing"
      (HC (Tagged _ m)) <- fetchLMMT lmmt
      case m of
        Dir  remoteDirContents -> do
          res <- traverse (uncurry $ localMissing . (path <>) . pure) $ M.toList remoteDirContents
          pure $ mconcat res
        File _ -> pure [Change path Del]


    remoteMissing' :: NonEmpty Path -> () -> m [Change (Term M)]
    remoteMissing' path () = remoteMissing path

    remoteMissing :: NonEmpty Path -> m [Change (Term M)]
    remoteMissing path = do
      let absolutePath = concatPath $ root <> path
      isFile <- liftIO $ doesFileExist absolutePath
      case isFile of
        True -> do
          contents <- liftIO $ readFile absolutePath
          pure [Change path $ Add $ Term $ Blob contents]
        False -> do
          isDir <- liftIO $ doesDirectoryExist absolutePath
          case isDir of
            False -> do
              pure [] -- not a dir or a file, shrug emoji
            True  -> do
              contents <- liftIO $ listDirectory' absolutePath
              mconcat <$> traverse (remoteMissing . (path <>) . pure) contents

