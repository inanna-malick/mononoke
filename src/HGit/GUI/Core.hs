{-# LANGUAGE TypeFamilies #-}

module HGit.GUI.Core where

--------------------------------------------
import           HGit.Core.Types
--------------------------------------------

data Focus x
  = SnapshotF (x 'SnapshotT)
  | FileTreeF (x 'FileTree)
  | CommitF   (x 'CommitT)
  | BlobF     (x 'BlobT)

type FocusWIPT m = Focus (WIPT m)
type FocusLMMT m = Focus (LMMT m)

-- (sing :: Sing i)
wrapFocus :: forall (i :: MTag) x. Sing i -> x i -> Focus x
wrapFocus s x = case s of
  SSnapshotT -> SnapshotF x
  SFileTree  -> FileTreeF x
  SCommitT   -> CommitF   x
  SBlobT     -> BlobF     x


data BranchFocus
  = MainBranch
  | OtherBranch String
  deriving (Eq, Ord, Show)
