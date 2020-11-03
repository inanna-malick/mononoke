
module HGit.GUI.Messages where


--------------------------------------------
import           Data.Functor.Const
import           Data.List.NonEmpty
--------------------------------------------
import           HGit.Core.Types.Render
import           HGit.Core.Types
import           HGit.GUI.Core
--------------------------------------------


data BrowserNavigation m
  = Focus (FocusWIPT m)


data UpdateMergeTrie m
  = ApplyChange (NonEmpty Path) (ChangeType (WIPT m))
  | RemoveChange (NonEmpty Path)
  | AddParent (LMMT m 'CommitT)
  | Reset
  | Finalize String -- finalize commit w/ message

instance Show (UpdateMergeTrie m) where
  show (ApplyChange p c) = "ApplyChange: " ++ (unlines $ renderChange $ cmapShim (Const . renderWIPT) $ Change p c)
  show (RemoveChange p) = "RemoveChange: " ++ show p
  show (AddParent _) = "AddParent: todo"
  show Reset = "Reset"
  show (Finalize msg) = "Finalize, msg: " ++ msg



data UpdateBranchState
  = ForkFrom BranchFocus String  -- branch off of current focus
  | DelBranch String
  | ChangeFocus BranchFocus -- blocked if IPC is Just


data SpawnPopup m
  = SpawnError String
  | SpawnRequestText
      String -- window name
      (String -> m ()) -- action to run given input (always string via text... could be better there)

