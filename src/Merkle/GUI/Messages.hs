{-# LANGUAGE RecordWildCards #-}

module Merkle.GUI.Messages
  ( module Merkle.GUI.Messages
  , SpawnPopup(..)
  , UpdateMergeTrie(..)
  ) where

--------------------------------------------
import           Merkle.GUI.Core
import           Merkle.GUI.Modal
import           Merkle.GUI.WorkingMergeTrie
--------------------------------------------


data BrowserNavigation m
  = Focus (FocusWIPT m)


data UpdateBranchState
  = ForkFrom BranchFocus String  -- branch off of current focus
  | DelBranch String
  | ChangeFocus BranchFocus -- blocked if IPC is Just


