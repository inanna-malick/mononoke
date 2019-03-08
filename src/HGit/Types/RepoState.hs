
module HGit.Types.RepoState (RepoState(..), initialRepoState) where

--------------------------------------------
import           Data.Aeson
import qualified Data.Map as M
import           GHC.Generics
--------------------------------------------
import           HGit.Serialization
import           HGit.Types.Common
import           HGit.Types.HGit
import           Merkle.Types (HashPointer(..))
import           Util.HRecursionSchemes
--------------------------------------------


data RepoState
  = RepoState
  { branches      :: M.Map BranchName (Const HashPointer 'CommitTag)
  , currentBranch :: BranchName
  } deriving (Generic)

initialRepoState :: RepoState
initialRepoState
  = RepoState
  { branches      = M.fromList [(initial, nullCommitHash)]
  , currentBranch = initial
  }
  where
    initial = "default"


instance ToJSON RepoState where
    toJSON (RepoState bs cb) =
      object
      [ "branches"      .= (M.map (unHashPointer . getConst) bs)
      , "currentBranch" .= cb
      ]
instance FromJSON RepoState where
    parseJSON = withObject "RepoState" $ \o -> do
      bs <- o .: "branches"
      cb <- o .: "currentBranch"
      pure $ RepoState (M.map (Const . HashPointer) bs) cb


