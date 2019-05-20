module Runtime.Types (RepoState(..), initialRepoState) where

--------------------------------------------
import           Data.Aeson
import qualified Data.Map as M
import           GHC.Generics
--------------------------------------------
import           HGit.Core.Types
--------------------------------------------


data RepoState
  = RepoState
  { branches      :: M.Map BranchName (Hash HashableCommit)
  , currentBranch :: BranchName
  } deriving (Generic)

initialRepoState :: Hash HashableCommit -> RepoState
initialRepoState initialHash
  = RepoState
  { branches      = M.fromList [(initial, initialHash)]
  , currentBranch = initial
  }
  where
    initial = "default"

instance ToJSON RepoState where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON RepoState
