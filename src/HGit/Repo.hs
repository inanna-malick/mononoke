module HGit.Repo (writeState, readState, mkStore, getBranch) where



--------------------------------------------
import           Control.Exception.Safe (MonadThrow, throw)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy as B
import qualified System.Directory as Dir
--------------------------------------------
import           Errors
import           HGit.Types
import           HGit.Store
import           HGit.Store.FileSystem (fsStore)
--------------------------------------------


hgitDir, hgitStateFile, hgitStoreDir :: PartialFilePath
hgitDir = ".hgit"
hgitStateFile = "state"
hgitStoreDir = "store"

-- TODO: dir tree traversal to allow for running app in non-root repo dir? mb
hgitDir', hgitState', hgitStore' :: MonadIO m => m FilePath
hgitDir'   = (++ "/" ++ hgitDir)       <$> liftIO Dir.getCurrentDirectory
hgitState' = (++ "/" ++ hgitStateFile) <$> hgitDir'
hgitStore' = (++ "/" ++ hgitStoreDir)  <$> hgitStore'

mkStore
  :: MonadIO m
  => MonadThrow m
  => m (Store m HGit)
mkStore = fsStore <$> hgitStore'


-- | get branch from state, fail if not found

getBranch
  :: MonadThrow m
  => BranchName
  -> RepoState
  -> m HashPointer
getBranch b
  = maybe (throw $ BranchNotFound b) pure . lookup b . branches

-- | Filesystem backed store using the provided dir
readState
  :: MonadIO m
  => MonadThrow m
  => m RepoState
readState = do
  path <- hgitState'
  contents <- liftIO $ B.readFile path
  case (AE.decode contents) of
    Nothing -> throw DecodeError
    Just x  -> do
      pure x

writeState
  :: MonadIO m
  => MonadThrow m
  => RepoState
  -> m ()
writeState rs = do
  path <- hgitState'
  liftIO . B.writeFile path $ AE.encode rs
