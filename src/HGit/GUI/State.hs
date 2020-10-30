{-# LANGUAGE RecordWildCards #-}

module HGit.GUI.State where


--------------------------------------------
import           Data.List.NonEmpty
import           Data.Map (Map)
import qualified Data.Map as Map
--------------------------------------------
import           HGit.Core.Types
import           HGit.GUI.Core
--------------------------------------------

data BranchState m
  = BranchState
  { bsMainBranch :: LMMT m 'CommitT
  , bsBranches :: [(String, LMMT m 'CommitT)]
  , bsFocus :: BranchFocus
  }

data InProgressCommit m
  = InProgressCommit
  { ipcMsg           :: String
  , ipcChanges       :: Map (NonEmpty Path) (ChangeType (WIPT m))
  , ipcParentCommits :: NonEmpty (WIPT m 'CommitT)
  }

asWIPTCommit :: InProgressCommit m -> WIPT m 'CommitT
asWIPTCommit InProgressCommit{..} = modifiedWIP $ Commit ipcMsg changes ipcParentCommits
  where
   changes = uncurry Change <$> Map.toList ipcChanges
