-- | Code for comparing two merkle trees in which any node can be
-- either a hash-addressed pointer to an entity in a remote store
-- or a direct representation of a hash-addressed entity
module Compare (diffMerkleDirs) where

--------------------------------------------
import           Control.Monad (join)
import qualified Data.HashMap.Strict as Map
import           Data.Functor.Const
--------------------------------------------
import           Diff.Types
import           Util.These (These(..), mapCompare)
import           Util.MyCompose
import           Util.HRecursionSchemes
import           HGit.Types
import           HGit.Store (Store)
import           HGit.Store.Deref (lazyDeref)
--------------------------------------------
import qualified Data.Functor.Compose as FC

-- | Diff two merkle trees, producing diffs and a record of expansions/derefs performed
--   lazily fetches structure of the two trees such that only the parts required
--   to do this comparison are fetched from the global state store (via 'network call')
diffMerkleDirs
  :: forall m
  -- no knowledge about actual monad stack - just knows it's the same
  -- as used by the store, which lets us create a lazy effectful streaming structure
   . Monad m
  => Store m HGit
  -> Const HashPointer 'DirTag -- top level interface is just pointers!
  -> Const HashPointer 'DirTag -- top level interface is just pointers!
  -> m [Diff]
diffMerkleDirs store mt1 mt2 =
  -- transform merkle trees (hash-addressed indirection) into lazily streaming data structures
  -- before passing to diffing alg
  diffMerkleDirs' (lazyDeref store mt1) (lazyDeref store mt2)


-- | Diff two merkle trees where the hash-identified nodes have been
--   converted to lazily-expanded effectful streams of values in some monadic stack 'm'
diffMerkleDirs'
  :: forall m
  -- no knowledge about actual monad stack - just knows it can
  -- sequence actions in it to deref successive layers (because monad)
   . Monad m
  => Term (FC.Compose (LazyHashTagged m) :++ HGit) 'DirTag
  -> Term (FC.Compose (LazyHashTagged m) :++ HGit) 'DirTag
  -- todo: writer w/ stack (?) so I can push/path segments to go with changes
  -> m [Diff]
diffMerkleDirs' dir1 dir2 = do
    ns1' <- dirEntries <$> derefLayer dir1
    ns2' <- dirEntries <$> derefLayer dir2

    fmap join . traverse resolveMapDiff
              $ mapCompare (Map.fromList ns1') (Map.fromList ns2')

  where
    resolveMapDiff
      :: ( PartialFilePath
         , These (DirPointer (Term (FC.Compose (LazyHashTagged m) :++ HGit)))
                 (DirPointer (Term (FC.Compose (LazyHashTagged m) :++ HGit)))
         )
      -> m [Diff]
    resolveMapDiff
      (n, This _) = pure [EntityDeleted n]
    resolveMapDiff
      (n, These e1 e2) = compareDerefed n e1 e2
    resolveMapDiff
      (n, That _) = pure [EntityCreated n]

    -- TODO: new name?
    compareDerefed
      :: PartialFilePath
      -> DirPointer (Term (FC.Compose (LazyHashTagged m) :++ HGit))
      -> DirPointer (Term (FC.Compose (LazyHashTagged m) :++ HGit))
      -> m [Diff]
    compareDerefed path (Left _) (Right _)
      = pure [DirReplacedWithFile path]
    compareDerefed path (Right _) (Left _)
      = pure [FileReplacedWithDir path]
    compareDerefed path (Right fc1) (Right fc2)
      | pointer fc1 /= pointer fc2 = pure [LeafModified path (pointer fc1) (pointer fc2)]
      | otherwise = pure []
    compareDerefed _    (Left dir1') (Left dir2')
      | pointer dir1' /= pointer dir2' = pure []
      | otherwise = diffMerkleDirs' dir1' dir2'



derefLayer
  :: forall f m
  . NatM m (Term (FC.Compose (LazyHashTagged m) :++ f))
            (f (Term (FC.Compose (LazyHashTagged m) :++ f)))
derefLayer (Term (HC (FC.Compose (C (_p, m))))) = m
