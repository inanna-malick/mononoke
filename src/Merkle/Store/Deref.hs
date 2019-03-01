module Merkle.Store.Deref where

--------------------------------------------
import           Util.MyCompose
import           Util.HRecursionSchemes
import           Merkle.Store
import           Merkle.Tree.Types
--------------------------------------------
import qualified Data.Functor.Compose as FC
import           Data.Functor.Const
import           Data.Singletons


-- | Greedily deref a merkle tree
-- NOTE: fully consumes potentially-infinite effectful stream and may not terminate
-- NOTE: Specialized to HGit for my convenience, could be generic wrt same but (TODO)
strictDeref
  :: forall i m
   . Monad m
  =>     Term (FC.Compose ((,) HashPointer :+ m) :++ HGit) i
  -> m $ Term (FC.Compose ((,) HashPointer     ) :++ HGit) i
strictDeref = getConst . cata alg
  where
    alg :: Alg (FC.Compose ((,) HashPointer :+ m) :++ HGit)
               (Const (m (Term (FC.Compose ((,) HashPointer :++ HGit)))))
    alg = undefined
    -- alg (HC (FC.Compose (C (p, effect)))) = Const $ do
    --   x <- effect
    --   case x of
    --     a@(Blob _) -> pure . Term . HC $ FC.Compose (p, a)


-- | construct a potentially-infinite tree-shaped stream of further values constructed by
-- deref-ing hash pointers using a hash-addressed store. Allows for store returning multiple
-- layers of tree structure in a single response (to enable future optimizations) via 'CoAttr'
-- TODO: update dox for gadt way

lazyDeref
  :: forall i m p
   . Monad m
  => SHFunctor p
  => HFunctor p
  => SingI i
  => Store m p
  -> HashPointer
  -> Term (FC.Compose (LazyHashTagged m) :++ p) i
lazyDeref store = sFutu alg . Const
  where
    alg :: SCVCoalg
             (FC.Compose (LazyHashTagged m) :++ p)
             (Const HashPointer)
    alg (Const p) = HC $ FC.Compose $ C (p, hfmap helper <$> sDeref store p)


    helper :: Term (FC.Compose HashIndirect :++ p)
                 :-> Context (FC.Compose (LazyHashTagged m) :++ p) (Const HashPointer)
    helper (Term (HC (FC.Compose (C (p, Nothing))))) = Hole $ Const p
    helper (Term (HC (FC.Compose (C (p, Just x))))) =
      Term $ HC (FC.Compose (C (p, pure $ hfmap helper x)))
