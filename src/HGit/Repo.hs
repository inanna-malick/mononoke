module HGit.Repo where -- (writeState, readState, mkStore, getBranch) where



--------------------------------------------
import           Control.Exception.Safe (MonadThrow, throw)
import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import qualified System.Directory as Dir
--------------------------------------------
import           Errors
import           HGit.Types.HGit
import           HGit.Types.RepoState
import           Merkle.Types
import           Merkle.Store
import           Merkle.Store.FileSystem (fsStore)
--------------------------------------------


hgitDir, hgitStateFile, hgitStoreDir :: PartialFilePath
hgitDir = ".hgit"
hgitStateFile = "state.json"
hgitStoreDir = "store"

-- TODO: dir tree traversal to allow for running app in non-root repo dir? mb
hgitDir', hgitState', hgitStore', baseDir :: MonadIO m => m FilePath
baseDir    = liftIO Dir.getCurrentDirectory
hgitDir'   = (++ "/" ++ hgitDir)       <$> baseDir
hgitState' = (++ "/" ++ hgitStateFile) <$> hgitDir'
hgitStore' = (++ "/" ++ hgitStoreDir)  <$> hgitDir'


mkHgitDir :: MonadIO m => m ()
mkHgitDir = do
  hgitDir' >>= liftIO . Dir.createDirectory
  storeDir <- hgitStore'
  liftIO $ Dir.createDirectory storeDir
  void $ traverse (liftIO . Dir.createDirectory . (\x -> storeDir ++ "/" ++ x)) ["dir", "blob", "commit"]

mkStore
  :: ( MonadIO      m
     , MonadThrow   m
     , AE.ToJSON1   f
     , AE.FromJSON1 f
     , Functor      f
     , Hashable     f
     )
  => String
  -> m (Store m f)
mkStore prefix = fsStore . (++ "/" ++ prefix) <$> hgitStore'

-- | get branch from state, fail if not found
getBranch
  :: MonadThrow m
  => BranchName
  -> RepoState
  -> m (Hash (Commit (Hash (Dir (Hash Blob)))))
getBranch b
  = maybe (throw $ BranchNotFound b) pure . M.lookup b . branches

-- | Filesystem backed store using the provided dir
readState
  :: MonadIO m
  => MonadThrow m
  => m RepoState
readState = do
  path <- hgitState'
  contents <- liftIO $ B.readFile path
  case (AE.eitherDecode contents) of
    Left e  -> throw $ DecodeError e
    Right x -> pure x

writeState
  :: MonadIO m
  => MonadThrow m
  => RepoState
  -> m ()
writeState rs = do
  path <- hgitState'
  liftIO . B.writeFile path $ AE.encode rs

data RepoCaps m
  = RepoCaps
  { _blobStore   :: Store m Blob
  , _dirStore    :: Store m HashableDir
  , _commitStore :: Store m HashableCommit
  }

mkCaps :: (MonadThrow m, MonadIO m) => m (RepoCaps m)
mkCaps = RepoCaps <$> mkStore "blob" <*> mkStore "dir" <*> mkStore "commit"
