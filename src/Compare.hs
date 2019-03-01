-- | Code for comparing two merkle trees in which any node can be
-- either a hash-addressed pointer to an entity in a remote store
-- or a direct representation of a hash-addressed entity
module Compare (compareMerkleTrees) where

--------------------------------------------
import           Control.Monad (join)
import qualified Data.HashMap.Strict as Map
import           Data.HashMap.Strict (HashMap)
import           Data.Functor.Const
import qualified Data.Set as Set
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
compareMerkleTrees
  :: forall m
  -- no knowledge about actual monad stack - just knows it's the same
  -- as used by the store, which lets us create a lazy effectful streaming structure
   . Monad m
  => Store m HGit
  -> HashPointer -- top level interface is just pointers!
  -> HashPointer -- top level interface is just pointers!
  -> m [Diff]
compareMerkleTrees store mt1 mt2 =
  -- transform merkle trees (hash-addressed indirection) into lazily streaming data structures
  -- before passing to diffing alg
  compareMerkleTrees' (lazyDeref store mt1) (lazyDeref store mt2)


-- | Diff two merkle trees where the hash-identified nodes have been
--   converted to lazily-expanded effectful streams of values in some monadic stack 'm'
compareMerkleTrees'
  :: forall m
  -- no knowledge about actual monad stack - just knows it can
  -- sequence actions in it to deref successive layers (because monad)
   . Monad m
  => Term (FC.Compose (LazyHashTagged m) :++ HGit) 'DirTag
  -> Term (FC.Compose (LazyHashTagged m) :++ HGit) 'DirTag
  -> m [Diff]
compareMerkleTrees' t1 t2
  | pointer t1 == pointer t2
      -- no diff, no need to explore further here
      = pure []
  | otherwise
      = do deref1 <- derefLayer t1
           deref2 <- derefLayer t2
           compareDerefed deref1 deref2

  where
    derefLayer
     :: forall f
      . NatM m (Term (FC.Compose (LazyHashTagged m) :++ f))
               (f (Term (FC.Compose (LazyHashTagged m) :++ f)))
    derefLayer (Term (HC (FC.Compose (C (_p, m))))) = m

    cmp :: HGit (Const HashPointer) 'DirTag
        -> HGit (Const HashPointer) 'DirTag
        -> Bool
    cmp (Dir n ps) (Dir n' ps') = n == n' && ps == ps'
    cmp (File n ps) (File n' ps') = n == n' && ps == ps'
    cmp _ _ = False

    compareDerefed
      :: HGit (Term (FC.Compose (LazyHashTagged m) :++ HGit)) 'DirTag
      -> HGit (Term (FC.Compose (LazyHashTagged m) :++ HGit)) 'DirTag
      -> m [Diff]
    compareDerefed entity1 entity2
      | dname entity1 /= dname entity2 = do
          -- flatten out sub-entities to only contain pointers then check equality
          if (hfmap pointer' entity1 `cmp` hfmap pointer' entity2)
            then
              pure [EntityRenamed (dname entity1) (dname entity2)]
            else
              pure [EntityDeleted $ dname entity1, EntityCreated $ dname entity2]
      | otherwise = do -- no name mismatch, but known hash mismatch - must explore further
          -- expansion for the case in which neither node is derefed or explored
          case (entity1, entity2) of
            (File _ fc1, File _ fc2)
              | pointer fc1 /= pointer fc2   -> pure [LeafModified (dname entity1, pointer fc1, pointer fc2)]
              -- ASSERTION we can only get here if there's a hash diff, but
              --            if we have a hash diff then the file contents should differ.
              -- NOTE: this indicates a situation where two hashes are /= but the hash-addressed
              --       file contents they describe are ==. fail silently (TODO: note error?)
              -- NOTE: making this function pure in 'm' is pretty nice, logging this error
              --       would require some additional error type that I don't want to think about
              --       right now
              | otherwise    -> pure []
            (Dir _ _, File _ _) -> pure [DirReplacedWithFile $ dname entity1]
            (File _ _, Dir _ _) -> pure [FileReplacedWithDir $ dname entity1]
            -- most of the complexity of this function is here - this
            -- is where the children of two nodes being diffed are compared.
            -- this requires derefing all nodes for which there is a hash
            -- mismatch and using the names of the resulting named entities to
            -- compare nodes with the same names
            (Dir _ ns1, Dir _ ns2) -> do
              let ns1Pointers = Set.fromList $ fmap pointer ns1
                  ns2Pointers = Set.fromList $ fmap pointer ns2

              -- DECISION: order of node children doesn't matter, so drop down to Set here
              let exploredNs1 = filter (not . flip Set.member (ns2Pointers) . pointer) ns1
                  exploredNs2 = filter (not . flip Set.member (ns1Pointers) . pointer) ns2

              derefedNs1 <- traverse derefLayer exploredNs1
              derefedNs2 <- traverse derefLayer exploredNs2

              let mkByNameMap :: [HGit (Term (FC.Compose (LazyHashTagged m) :++ HGit)) 'DirTag]
                              -> HashMap Name $ HGit (Term (FC.Compose (LazyHashTagged m) :++ HGit)) 'DirTag
                  mkByNameMap = Map.fromList . fmap (\e -> (dname e, e))
                  resolveMapDiff
                    (This (n, _)) = pure [EntityDeleted n]
                  resolveMapDiff
                    (These (_, a) (_, b)) = compareDerefed a b
                  resolveMapDiff
                    (That (n, _)) = pure [EntityCreated n]

              recurseRes <-
                traverse resolveMapDiff $ mapCompare (mkByNameMap derefedNs1) (mkByNameMap derefedNs2)

              let diffs = join recurseRes

              pure diffs
