{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module HGit.Core.MergeTrie where

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
import           HGit.Core.Types
import           Merkle.Higher.Types
import           Merkle.Higher.Store
import           Merkle.Higher.Store.Deref
import           Util.RecursionSchemes as R
import           Util.HRecursionSchemes as HR -- YOLO 420 SHINY AND CHROME
--------------------------------------------

-- NOTE: delete operations only valid on files, not directories





data NodeDeleted = NodeDeleted

data DirTrie a
  = DirTrie
  { _deleted  :: Maybe NodeDeleted -- only valid on files
  -- | updates to this blob. optionally tagged with source from previous snapshot (for merge case)
  , _updates  :: [(Maybe (Hash 'FileTree), Hash 'BlobT)]
  , _children :: Map Path a
  }
  deriving (Functor, Foldable, Traversable)

type MergeTrie m
  = Either (Term (LMM m) 'FileTree) -- recursion can terminate in untouched lazy merkle structure
  `Compose` DirTrie -- but it's mainly just DirTrie

-- NOTE FROM RAIN:
-- TWO PASSES, deletes first, then adds, then validates
-- NOTE: it's a set of changes on the trie


-- NOTE: called composite set in mononoke
-- NOTE: basically just multi-way merge
-- Q: how does this evaluate file -> dir or dir -> file merges? idk just represent in DirTree format
-- Q: how does DirTree format keep info needed for reconstruction of full File snapshot
--      specifically - commit last modified in (maybe unchanged for some merge output?)
--                   - last version of file (maybe multiple for multi-way merges)

-- fold a snapshot into a mergetrie
buildMergeTrie :: Term (LMM m) 'FileTree -> Fix (MergeTrie m) -> m (Fix (MergeTrie m))
buildMergeTrie lmm mt = undefined

-- - compare hashes, return input merge trie if no change

-- empty mergetrie
emptyMergeTrie :: Fix (MergeTrie m)
emptyMergeTrie = Fix . Compose . Right $ DirTrie Nothing [] Map.empty

-- Plan: first pass is n-way lazy diff. outputs above structure, with diff > initial ChangeType annotations
-- Q: can this represent file>dir and dir>file changes in merge? how to choose which is cannonical?
-- ANS: dir version is cannonical b/c needs recursive structure, file version represented as dir structure with list of add operations
-- ANS: does this imply that it should be _just_ a dir tree annotated with file add operations?
-- ANS: I think yes, it does
-- ANS: this also, I think, means it can be just Compose and not HCompose
--      file is non-recursive so can be just 'Add Hash'


-- Q: how to handle delete -> create -> delete change seq?

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
          let dt' = case _change of
                Del -> dt { _deleted = Just NodeDeleted }
                Add h -> dt { _updates = _updates dt ++ [(Nothing, h)] }
              dt'' = fst <$> dt'
           in pure $ Fix $ Compose $ Right $ dt'' -- apply
        f (Compose (Right dt)) (path:paths) = case Map.lookup path (_children dt) of
              Just (_, next) -> next paths -- sub-path exists, recurse
              Nothing -> let mt = constructMT _change paths
                             children' = Map.insert path mt $ fst <$> _children dt
                             dt' = dt { _children = children' }
                          in pure $ Fix $ Compose $ Right dt'

-- helper function, constructs merge trie with change at path
constructMT :: ChangeType Hash -> [Path] -> Fix (MergeTrie m)
constructMT changeType = R.ana f
  where f :: [Path] -> MergeTrie m [Path]
        f [] = Compose $ Right $ case changeType of
          Del -> DirTrie (Just NodeDeleted) [] Map.empty
          Add h -> DirTrie Nothing [(Nothing, h)] Map.empty
        f (x:xs) = Compose $ Right $ DirTrie Nothing [] (Map.singleton x xs)

-- results in 'm' because it may need to expand the provided tree (which could afterall just be a hash)
applyChangeH
  :: forall m
   . Monad m
  => [Path]
  -> ChangeType Hash
  -> Term (LMM m) 'FileTree -- NOTE: is there a wy to just write a traversal over the one type? probably not
  -- actually wait, mergeTrie _requires_ 'FileTree, which is a problem
  -- would need to `error` on all non-matching branches
  -- actually, could solve by using manual recursion - just can't use hcata
  -> m (Fix (MergeTrie m))
applyChangeH fullPath changeType (Term (HC (Tagged hash (HC (Compose m))))) = do
  m' <- m
  case m' of
    Dir children -> let children' :: Map Path (Fix (MergeTrie m))
                        children' = Fix . Compose . Left <$> children
                     in case fullPath of
      [] -> do -- delete/add hash on dir, questionably valid/invalid assuming no prior delete?
        let dt :: DirTrie (Fix (MergeTrie m))
            dt = case changeType of
              Del -> DirTrie
                  { _deleted = Just NodeDeleted
                  , _updates = []
                  , _children = children'
                  }
              Add h -> DirTrie
                  { _deleted = Nothing
                  , _updates = [(Nothing, h)]
                  , _children = children'
                  }

        pure $ Fix $ Compose $ Right $ dt

      (path:paths) -> do
        -- construct new child or recurse into (and update) existing
        child <- case Map.lookup path children of
          Nothing -> pure $ constructMT changeType (path:paths)
          Just next -> applyChangeH paths changeType next

        let dt :: DirTrie (Fix (MergeTrie m))
            dt = DirTrie
                  { _deleted = Nothing
                  , _updates = []
                  , _children = Map.insert path child children'
                  }

        pure $ Fix $ Compose $ Right $ dt

    -- TODO: how do I persist the lastMod/prev/etc content?
    File blob lastMod prev -> case fullPath of
      [] -> do
        let dt :: DirTrie (Fix (MergeTrie m))
            dt = case changeType of
              Del -> DirTrie
                  { _deleted = Just NodeDeleted
                  , _updates = [(Just hash, _tag . getHC . unTerm $ blob)]
                  , _children = Map.empty
                  }
              Add h -> DirTrie
                  { _deleted = Nothing
                  , _updates = [(Just hash, _tag . getHC . unTerm $ blob), (Nothing, h)]
                  , _children = Map.empty
                  }

        pure $ Fix $ Compose $ Right $ dt
      (path:paths) -> do
        let dt :: DirTrie (Fix (MergeTrie m))
            dt = DirTrie
                  { _deleted = Nothing
                  , _updates = [(Just hash, _tag . getHC . unTerm $ blob)]
                  , _children = Map.singleton path (constructMT changeType paths)
                  }

        pure $ Fix $ Compose $ Right $ dt


-- if hash at path == [] is == to hash of change (if file add op) return unchanged else expand and etc

type MergeError = Text
-- update merge-in-progress data structure
validateMerge
  :: Fix (MergeTrie m)
  -> MergeError `Either` Fix (MergeTrie m)
validateMerge = undefined
