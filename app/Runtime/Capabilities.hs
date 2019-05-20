module Runtime.Capabilities where


--------------------------------------------
import           Control.Exception.Safe (MonadThrow, throwString)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader
import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import qualified System.Directory as Dir
--------------------------------------------
import           HGit.Core.Types
import           Runtime.Types
--------------------------------------------


hgitStateFile :: PartialFilePath
hgitStateFile = ".hgit-state.json"

-- TODO: dir tree traversal to allow for running app in non-root repo dir? mb
hgitState', hgitBaseDir :: MonadIO m => m FilePath
hgitBaseDir    = liftIO Dir.getCurrentDirectory
hgitState' = (++ "/" ++ hgitStateFile) <$> hgitBaseDir

-- | get branch from state, fail if not found
getBranch
  :: MonadThrow m  => MonadReader (RepoCaps m') m
  => BranchName -> m (Hash HashableCommit)
getBranch bn
  = asks rcState >>= getBranch' bn

-- | get branch from state, fail if not found
getBranch'
  :: MonadThrow m => BranchName -> RepoState -> m (Hash HashableCommit)
getBranch' bn
  = maybe (throwString $ "branch not found: " ++ bn) pure . M.lookup bn . branches

-- | Filesystem backed store using the provided dir
-- fails if not present (todo check/confirm)
readState
  :: MonadIO m
  => MonadThrow m
  => m RepoState
readState = do
  path <- hgitState'
  liftIO $ AE.eitherDecodeFileStrict path >>= \case
    Left e  -> throwString e
    Right x -> pure x

writeState
  :: MonadIO m
  => MonadThrow m
  => RepoState
  -> m ()
writeState rs = do
  path <- hgitState'
  liftIO . B.writeFile path $ AE.encode rs


-- FIXME: just need to wire in IPFS host/port parsers here
data HgitStore m
  = HgitStore
  { _blobStore   :: Store m Blob
  , _dirStore    :: Store m HashableDir
  , _commitStore :: Store m HashableCommit
  }

-- withFallbackRC :: Monad m => HgitStore m -> HgitStore m -> HgitStore m
-- withFallbackRC main fallback
--   = HgitStore
--   { _blobStore   = withFallback (_blobStore main)   (_blobStore fallback)
--   , _dirStore    = withFallback (_dirStore main)    (_dirStore fallback)
--   , _commitStore = withFallback (_commitStore main) (_commitStore fallback)
--   }

-- TODO: create dir structure?
-- mkLocalCaps :: IO (HgitStore IO)
-- mkLocalCaps = HgitStore <$> mkStore "blob" <*> mkStore "dir" <*> mkStore "commit"
--   where mkStore prefix = fsStore . (++ "/" ++ prefix) <$> hgitStore'


data RepoCaps m
  = RepoCaps
  { rcStore   :: HgitStore m
  , rcState   :: RepoState
  , rcBaseDir :: FilePath
  }
