{-# LANGUAGE RecordWildCards #-}

module Merkle.GUI.State where


--------------------------------------------
import           Data.List.NonEmpty
import           Data.Map (Map)
import qualified Data.Map as Map
--------------------------------------------
import           Merkle.Bonsai.Types
import           Merkle.GUI.Core
--------------------------------------------

data BranchState m
  = BranchState
  { bsMainBranch :: LMMT m 'CommitT
  , bsBranches :: [(String, LMMT m 'CommitT)]
  , bsFocus :: BranchFocus
  }


instance Show (BranchState m) where
  show BranchState{..} = mconcat
    [ "BranchState { main: \n"
    , show $ hashOfLMMT bsMainBranch
    , "\n, branches: \n"
    , show $ fmap (fmap hashOfLMMT) bsBranches
    , "\n, focus: \n"
    , show bsFocus
    ]

data InProgressCommit m
  = InProgressCommit
  { ipcChanges       :: Map (NonEmpty Path) (ChangeType (WIPT m))
  , ipcParentCommits :: NonEmpty (WIPT m 'CommitT)
  }

asWIPTCommit :: String -> InProgressCommit m -> WIPT m 'CommitT
asWIPTCommit msg InProgressCommit{..} = modifiedWIP $ Commit msg changes ipcParentCommits
  where
   changes = uncurry Change <$> Map.toList ipcChanges
