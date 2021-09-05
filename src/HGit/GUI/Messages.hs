{-# LANGUAGE RecordWildCards #-}

module HGit.GUI.Messages
  ( module HGit.GUI.Messages
  , SpawnPopup(..)
  , UpdateMergeTrie(..)
  ) where

--------------------------------------------
import           HGit.GUI.Core
import           HGit.GUI.Modal
import           HGit.GUI.WorkingMergeTrie
--------------------------------------------


data BrowserNavigation m
  = Focus (FocusWIPT m)


data UpdateBranchState
  = ForkFrom BranchFocus String  -- branch off of current focus
  | DelBranch String
  | ChangeFocus BranchFocus -- blocked if IPC is Just


