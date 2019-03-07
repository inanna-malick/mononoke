
module HGit.Types.RepoState (RepoState(..), initialRepoState) where

--------------------------------------------
import           Data.Aeson
import           Data.Functor.Const (Const(..))
import qualified Data.Map as M
import           GHC.Generics
--------------------------------------------
import           HGit.Serialization
import           HGit.Types.Common
import           Merkle.Types (HashPointer(..))
--------------------------------------------


data RepoState
  = RepoState
  { branches      :: M.Map BranchName HashPointer
  , currentBranch :: BranchName
  } deriving (Generic)

initialRepoState :: RepoState
initialRepoState
  = RepoState
  { branches      = M.fromList [(initial, getConst $ nullCommitHash)]
  , currentBranch = initial
  }
  where
    initial = "default"


instance ToJSON RepoState where
    toJSON (RepoState bs cb) =
      object
      [ "branches"      .= (M.map unHashPointer bs)
      , "currentBranch" .= cb
      ]
instance FromJSON RepoState where
    parseJSON = withObject "RepoState" $ \o -> do
      bs <- o .: "branches"
      cb <- o .: "currentBranch"
      pure $ RepoState (M.map HashPointer bs) cb


