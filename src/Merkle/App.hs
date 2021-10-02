{-# LANGUAGE TypeFamilies #-}
{-# language ScopedTypeVariables        #-}



module Merkle.App where


import           Merkle.Bonsai.Types
import           Merkle.Generic.HRecursionSchemes

import           System.Directory
import qualified Data.Map.Merge.Strict as M
import qualified Data.Map.Strict as M
import           Control.Monad.Except
import           Data.List.NonEmpty (NonEmpty)




empty :: Applicative m => LMMT m 'FileTree
empty = liftLMMT $ Term $ Dir M.empty



-- commands to implement:
-- mkbranch: create branch
-- delbranch: delete branch
-- checkout: impose remote state on local subdir (note to self: use containers to test this)
-- commit: commit all state in repo with message


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
  { backing_store_addr :: String
  , backing_store_port :: Int
  , currentCommit      :: Hash 'CommitT
  , currentBranch      :: Maybe String
  , snapshotMappings   :: M.Map (Hash 'CommitT) (Hash 'SnapshotT)
  , branches           :: M.Map String (Hash 'CommitT)
  } deriving (Ord, Eq, Show)

-- simple flow: add some files, git commit, branch, same, etc


-- initialLocalState :: LocalState
-- initialLocalState
--   = LocalState
--   { backing_store_addr = "localhost"
--   , backing_store_port = 8080
--   , snapshotMappings   = M.empty
--   , branches           = M.empty
--   , currentCommit      = empty_hash
--   , currentBranch      = "main"
--   }




-- commit
--   :: forall m
--    . MonadError String m
--   => MonadIO m
--   => Path
--   -> String
--   -> m [Change (Term M)]
-- commit root commitMsg = do
--   localState <- readLocalState
--   client     <- mkGRPCClient
--   let dagStore = mkDagStore client
--   snapshotLMMT <- case M.lookup (currentCommit localState) (snapshotMappings localState) of
--     Nothing -> do
--       snapshot <- buildSnapshot
--       snaspshotHash <- upload snapshot dagStore
--       pure $ liftLMMT snapshot
--     Just snapshotHash -> dp
--       pure $ expandHash snapshotHash
--   diffs <- diffLocalState root 




-- working, could do with some polish and tests (lmao) and etc
diffLocalState
  :: forall m
   . MonadError String m
  => MonadIO m
  => Path
  -> LMMT m 'FileTree
  -> m [Change (Term M)]
diffLocalState root snapshot = processRoot snapshot
  where
    processRoot :: LMMT m 'FileTree -> m [Change (Term M)]
    processRoot lmmt = do
      liftIO $ putStrLn "processRoot"
      contents <- liftIO $ listDirectory root
      let local = M.fromList $ fmap (\a -> (a, ())) contents
      remote <- do
        (HC (Tagged _ m)) <- fetchLMMT lmmt
        case m of
          File _ -> throwError "expected root path to be a dir in LMMT"
          Dir  d -> pure d
      mconcat . fmap snd . M.toList <$>
        M.mergeA (M.traverseMissing $ remoteMissing' . (pure root <>) . pure)
                 (M.traverseMissing $ localMissing   . (pure root <>) . pure)
                 (M.zipWithAMatched $ bothPresent    . (pure root <>) . pure)
                  local remote


    -- both paths present, file/dir conflict still possible
    bothPresent :: NonEmpty Path -> () -> LMMT m 'FileTree -> m [Change (Term M)]
    bothPresent path () lmmt = do
      liftIO $ putStrLn "bothPresent"
      let path' = concatPath path
      (HC (Tagged _ m)) <- fetchLMMT lmmt
      case m of
        Dir  remoteDirContents  -> do
          isDir <- liftIO $ doesDirectoryExist path'
          case isDir of
            False -> do -- remote dir, local ???, if file then add/remote del, else ???
              isFile <- liftIO $ doesFileExist path'
              case isFile of
                False -> throwError $ "Dir replaced with non-file type entity at " ++ show path
                True  -> do -- remote dir, local file
                  remoteDeletes <- traverse (uncurry $ localMissing . (path <>) . pure)
                             $ M.toList remoteDirContents
                  localContents <- liftIO $ readFile path'
                  pure $ [Change path $ Add $ Term $ Blob localContents] ++ mconcat remoteDeletes

            True  -> do
              contents <- liftIO $ listDirectory path'
              let local = M.fromList $ fmap (\a -> (a, ())) contents
              mconcat . fmap snd . M.toList <$>
                M.mergeA (M.traverseMissing $ remoteMissing' . (path <>) . pure)
                         (M.traverseMissing $ localMissing   . (path <>) . pure)
                         (M.zipWithAMatched $ bothPresent    . (path <>) . pure)
                          local remoteDirContents

        File remoteContentsLMMT -> do
          isFile <- liftIO $ doesFileExist path'
          case isFile of
            True -> do -- diff contents. potential optimization, hash local before blob fetch.
              (HC (Tagged _ remoteBlob)) <- fetchLMMT $ sfBlob remoteContentsLMMT
              let (Blob remoteContents) = remoteBlob
              localContents <- liftIO $ readFile path'
              case localContents == remoteContents of
                True -> pure [] -- no change
                False -> pure [Change path $ Add $ Term $ Blob localContents]
            False -> do
              localChanges <- do
                isDir <- liftIO $ doesDirectoryExist path'
                case isDir of
                  False -> pure [] -- not a dir or a file, shrug emoji
                  True  -> do
                    contents <- liftIO $ listDirectory path'
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
      let path' = concatPath path
      isFile <- liftIO $ doesFileExist path'
      case isFile of
        True -> do
          contents <- liftIO $ readFile path'
          pure [Change path $ Add $ Term $ Blob contents]
        False -> do
          isDir <- liftIO $ doesDirectoryExist path'
          case isDir of
            False -> do
              pure [] -- not a dir or a file, shrug emoji
            True  -> do
              contents <- liftIO $ listDirectory path'
              mconcat <$> traverse (remoteMissing . (path <>) . pure) contents

