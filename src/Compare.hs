-- | Code for comparing two merkle trees in which any node can be
-- either a hash-addressed pointer to an entity in a remote store
-- or a direct representation of a hash-addressed entity
module Compare (compareMerkleTrees) where

--------------------------------------------
import           Control.Monad (join)
import qualified Data.HashMap.Strict as Map
import           Data.HashMap.Strict (HashMap)
import qualified Data.Set as Set
--------------------------------------------
import           Diff.Types
import           Util.These (These(..), mapCompare)
import           Util.MyCompose
import           Util.RecursionSchemes
import           Merkle.Types
import           Merkle.Tree.Encoding -- todo shuffle types around weird that this is here
import           Merkle.Tree.Types
import           Merkle.Store (Store)
import           Merkle.Store.Deref (lazyDeref)
--------------------------------------------

-- TODO: use underlying monad stack to track derefs instead ofdoing so inline - more concise!

-- | Diff two merkle trees, producing diffs and a record of expansions/derefs performed
--   lazily fetches structure of the two trees such that only the parts required
--   to do this comparison are fetched from the global state store (via 'network call')
compareMerkleTrees
  :: forall m
  -- no knowledge about actual monad stack - just knows it's the same
  -- as used by the store, which lets us create a lazy effectful streaming structure
   . Monad m
  -- TODO: weird type? w/e, just compare shallow merkle trees for now.. later compare blobs..
  => Store m (Named :+ Tree ShallowMerkleTreeLayer)
  -> Pointer -- top level interface is just pointers!
  -> Pointer -- top level interface is just pointers!
  -> m [Diff] -- resulting diffs
compareMerkleTrees store1 mt1 mt2 =
  -- transform merkle trees (hash-addressed indirection) into lazily streaming data structures
  -- before passing to diffing alg
  compareMerkleTrees' (lazyDeref store1 mt1) (lazyDeref store1 mt2)


-- | Diff two merkle trees where the hash-identified nodes have been
--   converted to lazily-expanded effectful streams of values in some monadic stack 'm'
compareMerkleTrees'
  :: forall m
  -- no knowledge about actual monad stack - just knows it can
  -- sequence actions in it to deref successive layers (because monad)
   . Monad m
  => Fix $ WithHash :+ m :+ Named :+ Tree ShallowMerkleTreeLayer
  -> Fix $ WithHash :+ m :+ Named :+ Tree ShallowMerkleTreeLayer
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
    compareDerefed
      :: Named :+ Tree ShallowMerkleTreeLayer $ Fix $ WithHash :+ m :+ Named :+ Tree ShallowMerkleTreeLayer
      -> Named :+ Tree ShallowMerkleTreeLayer $ Fix $ WithHash :+ m :+ Named :+ Tree ShallowMerkleTreeLayer
      -> m [Diff]
    compareDerefed (C (name1, entity1)) (C (name2, entity2))
      | name1 /= name2 =
          -- flatten out sub-entities to only contain pointers then check equality
          if (fmap pointer entity1 == fmap pointer entity2)
            then
              pure [EntityRenamed name1 name2]
            else
              pure [EntityDeleted name1, EntityCreated name2]
      | otherwise = -- no name mismatch, but known hash mismatch - must explore further
          -- expansion for the case in which neither node is derefed or explored
          case (entity1, entity2) of
            (Leaf fc1, Leaf fc2)
              | fc1 /= fc2   -> pure [LeafModified name1 fc1 fc2]
              -- ASSERTION we can only get here if there's a hash diff, but
              --            if we have a hash diff then the file contents should differ.
              -- NOTE: this indicates a situation where two hashes are /= but the hash-addressed
              --       file contents they describe are ==. fail silently (TODO: note error?)
              -- NOTE: making this function pure in 'm' is pretty nice, logging this error
              --       would require some additional error type that I don't want to think about
              --       right now
              | otherwise    -> pure []
            (Node _, Leaf _) -> pure [DirReplacedWithFile name1]
            (Leaf _, Node _) -> pure [FileReplacedWithDir name1]
            -- most of the complexity of this function is here - this
            -- is where the children of two nodes being diffed are compared.
            -- this requires derefing all nodes for which there is a hash
            -- mismatch and using the names of the resulting named entities to
            -- compare nodes with the same names
            (Node ns1, Node ns2) -> do
              let ns1Pointers = Set.fromList $ fmap pointer ns1
                  ns2Pointers = Set.fromList $ fmap pointer ns2

              -- DECISION: order of node children doesn't matter, so drop down to Set here
              let exploredNs1 = filter (not . flip Set.member (ns2Pointers) . pointer) ns1
                  exploredNs2 = filter (not . flip Set.member (ns1Pointers) . pointer) ns2
                  -- for construting 'unexpanded' branches
                  -- unexploredNs1 :: [Fix $ WithHash :+ Maybe :+ Named :+ Tree]
                  -- unexploredNs1  = fmap (unexpanded . pointer) $ filter (flip Set.member (ns2Pointers) . pointer) ns1
                  -- unexploredNs2 :: [Fix $ WithHash :+ Maybe :+ Named :+ Tree]
                  -- unexploredNs2  = fmap (unexpanded . pointer) $ filter (flip Set.member (ns1Pointers) . pointer) ns2

              derefedNs1 <- traverse (\x -> fmap C . fmap (pointer x,) $ derefLayer x) exploredNs1
              derefedNs2 <- traverse (\x -> fmap C . fmap (pointer x,) $ derefLayer x) exploredNs2

              let mkByNameMap
                    :: forall x
                     . [WithHash :+ Named :+ Tree x $ Fix $ WithHash :+ m :+ Named :+ Tree x]
                    -> HashMap Name $ WithHash :+ Named :+ Tree x
                                    $ Fix $ WithHash :+ m :+ Named :+ Tree x
                  mkByNameMap ns = Map.fromList $ fmap (\e@(C (_, C (n, _))) -> (n, e)) ns
                  resolveMapDiff
                    (This (n,_)) = pure [EntityDeleted n]

                  resolveMapDiff
                    (These (_, C (_,a)) (_, C (_,b))) = compareDerefed a b

                  resolveMapDiff
                    (That (n,_)) = pure [EntityCreated n]

              res <- traverse resolveMapDiff $ mapCompare (mkByNameMap derefedNs1) (mkByNameMap derefedNs2)

              pure $ join res


              -- kinda gross - basically just taking all the results and hammering them into the right shape
              -- let diffs = join $ fmap fst recurseRes

              -- pure diffs


derefLayer :: forall m f
            . Fix $ WithHash :+ m :+ f
           -> m $ f $ Fix $ WithHash :+ m :+ f
derefLayer   = getCompose . snd . getCompose . unfix
