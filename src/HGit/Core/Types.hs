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

-- TODO: hash type which allows for inline? maybe.
-- TODO: use case is commit comments, could point to a blob or have inline

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


-- IDEA: hash visualization system, using simple shapes + colors, eg [blue square, orange triangle, ..]
-- IDEA: for compact visualization of first N bytes of hashes. follow up later, could be fun terminal output

-- for use while running merges

data DirTrie a
  = DirTrie
  { _changes:: [ChangeType Hash]
  , _children:: Map Path a
  }
  deriving (Functor, Foldable, Traversable)


-- work-in-progress structure for running merges

type MergeTrie m
  = Either (Term (LMM m) 'FileTree) -- recursion can terminate in untouched lazy merkle structure
  `Compose` DirTrie -- but it's mainly just DirTrie

-- NOTE FROM RAIN:
-- TWO PASSES, deletes first, then adds, then validates
-- NOTE: it's a set of changes on the trie


-- NOTE: called composite set in mononoke
buildMergeTrie :: [Term (LMM m) 'FileTree] -> m (Fix (MergeTrie m))
buildMergeTrie = undefined

-- Plan: first pass is n-way lazy diff. outputs above structure, with diff > initial ChangeType annotations
-- Q: can this represent file>dir and dir>file changes in merge? how to choose which is cannonical?
-- ANS: dir version is cannonical b/c needs recursive structure, file version represented as dir structure with list of add operations
-- ANS: does this imply that it should be _just_ a dir tree annotated with file add operations?
-- ANS: I think yes, it does
-- ANS: this also, I think, means it can be just Compose and not HCompose
--      file is non-recursive so can be just 'Add Hash'

-- results in 'm' because it may need to expand the provided tree (which could afterall just be a hash)
applyChange
  :: forall m
   . Monad m
  => Change Hash
  -> Fix (MergeTrie m)
  -> m (Fix (MergeTrie m))
applyChange c@Change{..} t = para f t $ toList _path
  where f :: MergeTrie m ( Fix (MergeTrie m)
                         , [Path] -> m (Fix (MergeTrie m))
                         )
          -> [Path]
          -> m (Fix (MergeTrie m))
        f (Compose (Left  mt)) xs = applyChangeH xs _change mt -- expand and apply
        f (Compose (Right dt)) [] =
          let changes' = _changes dt ++ [_change] -- TODO: validate, simplify list of changes if needed
              dt' = DirTrie changes' (fst <$> _children dt)
           in pure $ Fix $ Compose $ Right $ dt' -- apply
        f (Compose (Right dt)) (path:paths) = case Map.lookup path (_children dt) of
              Just (_, next) -> next paths -- sub-path exists, recurse
              Nothing -> let mt = R.ana builddt paths
                             builddt :: [Path] -> MergeTrie m [Path]
                             builddt [] = Compose $ Right $ DirTrie [_change] Map.empty
                             builddt (x:xs) = Compose $ Right $ DirTrie [] (Map.singleton x xs)
                             children' = Map.insert path mt $ fst <$> _children dt
                             dt' = DirTrie (_changes dt) (children')
                          in pure $ Fix $ Compose $ Right dt'


-- results in 'm' because it may need to expand the provided tree (which could afterall just be a hash)
applyChangeH
  :: Monad m
  => [Path]
  -> ChangeType Hash
  -> Term (LMM m) 'FileTree -- NOTE: is there a wy to just write a traversal over the one type? probably not
  -- actually wait, mergeTrie _requires_ 'FileTree, which is a problem
  -- would need to `error` on all non-matching branches
  -- actually, could solve by using manual recursion - just can't use hcata
  -> m (Fix (MergeTrie m))
applyChangeH path changeType (Term (HC (Tagged hash (HC (Compose m))))) = do
  m' <- m
  case m' of
    Dir children -> case path of
      [] ->  undefined -- dir -> file operation, reject?
      (x:xs) -> undefined -- dir -> dir operation, allow, recurse

    -- TODO: how do I persist the lastMod/prev/etc content?
    File blob lastMod prev -> case path of
      [] ->  undefined -- file -> file operation, delete or add
      (x:xs) ->  undefined -- file -> dir operation, reject?


-- if hash at path == [] is == to hash of change (if file add op) return unchanged else expand and etc

type MergeError = Text
-- update merge-in-progress data structure
validateMerge
  :: Fix (MergeTrie m)
  -> MergeError `Either` Fix (MergeTrie m)
validateMerge = undefined

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
