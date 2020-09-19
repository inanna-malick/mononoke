{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module HGit.Core.Types where

--------------------------------------------
import           Data.Aeson as AE
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import           Data.Functor.Compose
import           Data.List.NonEmpty (NonEmpty, toList)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import           Data.Text.Encoding (decodeLatin1, encodeUtf8)
import           Data.Singletons.TH
import           GHC.Generics
--------------------------------------------
import           Merkle.Higher.Types
import           Merkle.Higher.Store
import           Merkle.Higher.Store.Deref
import           Util.RecursionSchemes as R
import           Util.HRecursionSchemes as HR -- YOLO 420 SHINY AND CHROME
--------------------------------------------

$(singletons [d|
  data MTag = SnapshotT | FileTree | CommitT | BlobT
 |])

type Path = Text

data Change a
  = Change
  { _path::   NonEmpty Path
  , _change:: ChangeType a
  }

data ChangeType a
  = Add (a 'BlobT)
  | Del

data M a i where
  -- snapshots:
  Snapshot
    :: a 'FileTree -- snapshot of file tree in commit
    -> a 'CommitT -- originating commit
    -> M a 'SnapshotT

  -- file tree entries:
  File
    :: a 'BlobT     -- file blob
    -> a 'CommitT  -- last modified in this commit
    -> a 'FileTree -- previous incarnation
    -> M a 'FileTree

  Dir
    --  TODO: will need canonical on-disk map repr/cannonical hash
    :: Map Path (a 'FileTree) -- children
 -- Q: do I store this here, too, or do I only store it for files?
 --   -> a 'CommitT -- last modified in this commit
 --   -> a 'FileTree -- previous incarnation
    -> M a 'FileTree

  -- commits:
  NullCommit
    :: M a 'CommitT

  Commit
    :: Text        -- commit message
    -> NonEmpty (Change a)  -- list of inline changes
    -> NonEmpty (a 'CommitT) -- parent commits
    -> M a 'CommitT

  -- blobs:
  Blob
    :: ByteString
    -> M a 'BlobT



-- Lazy Merkle M
type LMM m = Tagged Hash `HCompose` Compose m `HCompose` M

-- Merkle M with type level distinction betweeen preexisting persisted content that may be expanded (LMM) and fresh content that has not been persisted (just M)
-- goal is to model applying multiple rounds of updates/changes to a structure
-- NOTE: can I abstract over this model? would be nice to have utilities for working with it
type WIPM m = HEither (Term (LMM m)) `HCompose` M


instance ExtractKeys M where
  extractHashKeys (Snapshot tree orig) = [unHash tree, unHash orig]
  extractHashKeys (File blob lastMod prev) = [unHash blob, unHash lastMod, unHash prev]
  extractHashKeys (Dir children) = unHash . snd <$> Map.toList children
  extractHashKeys NullCommit = []
  extractHashKeys (Commit _ changes parents) =
    let unChangeHash (Add h) = [unHash h]
        unChangeHash  Del    = []
     in (toList changes >>= (unChangeHash . _change)) ++ toList (fmap unHash parents)
  extractHashKeys (Blob _) = []


-- TODO: investigate a bit? can I derive this?
instance HFunctor M where
  hfmap f _ = undefined

instance HTraversable M where
  hmapM f _ = undefined
