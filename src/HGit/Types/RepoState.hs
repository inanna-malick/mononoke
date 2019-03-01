
module HGit.Types.RepoState (RepoState(..)) where

--------------------------------------------
import           Data.Aeson
import           GHC.Generics
--------------------------------------------
import           HGit.Types.Common
--------------------------------------------

data RepoState
  = RepoState
  { branches      :: [(BranchName, HashPointer)]
  , currentBranch :: BranchName
  } deriving (Generic)

instance ToJSON RepoState where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON RepoState
