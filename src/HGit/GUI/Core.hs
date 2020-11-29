{-# LANGUAGE TypeFamilies #-}

module HGit.GUI.Core where

--------------------------------------------
import           Data.Singletons
-- import           Reactive.Threepenny (Event, Handler)
--------------------------------------------
import           HGit.Core.Types
--------------------------------------------


-- class CanRegisterHandler e m where
--   register :: Handler e -> m ()


-- class CanDispatchEvent e m where
--   dispatch :: e -> m ()


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
