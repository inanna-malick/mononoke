{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}

module Util.Util where -- this module name is a sin against god, todo something descriptive


import           Control.Monad.Except
import           Data.List (intercalate)

import qualified Data.Comp.Multi.Algebra as C
import qualified Data.Comp.Multi.HFunctor as C
import           Data.Comp.Multi.HFunctor ((:->))
import qualified Data.Comp.Multi.Term as C

import           Util.MyCompose
import           Data.Functor.Const
import qualified Data.Functor.Compose as FC


mapErrUtil :: Functor m => (e -> e') -> ExceptT e m a -> ExceptT e' m a
mapErrUtil f = mapExceptT (fmap (either (Left . f) Right))

type Name = String
type FileChunk = String

data DirTag
data MetaTag
data CommitTag

data HGit a i where
  -- file chunk bits
  Leaf :: FileChunk -> HGit a FileChunk
  Blob :: [a FileChunk] -> HGit a FileChunk

  -- dir and file bits
  File :: Name -> a FileChunk -> HGit a DirTag
  Dir :: Name -> [a DirTag] -> HGit a DirTag

  -- TODO/IDEA: can fit entities like branches and change lists (maybe?) in here
                         -- maybe w/ struct. sharing instead..

  -- commits
  Commit :: Name -> a DirTag -> HGit a CommitTag

  -- branches
  Branch :: Name -> a CommitTag -> HGit a MetaTag

instance C.HFunctor HGit where
  hfmap _ (Leaf fc)      = Leaf fc
  hfmap f (Blob fcs)     = Blob $ fmap f fcs
  hfmap f (File n fc)    = File n $ f fc
  hfmap f (Dir n dcs)    = Dir n $ fmap f dcs
  hfmap f (Commit n cc)  = Commit n $ f cc
  hfmap f (Branch n bc)  = Branch n $ f bc

type MyHGit  = C.Term HGit DirTag
type MyBlobTree = C.Term HGit FileChunk

leaf :: FileChunk -> MyBlobTree
leaf =
  C.Term . Leaf

blobTree :: MyBlobTree
blobTree = C.Term (Blob [leaf "foo", leaf "bar", leaf "baz"])

dir :: [MyHGit] -> MyHGit
dir xs = C.Term (Dir "mydir" xs)

file :: MyBlobTree -> MyHGit
file x = C.Term (File "myfile" x)

testDir :: MyHGit
testDir = dir [file blobTree, dir [file blobTree]]

printCata :: forall i. C.Term HGit i -> String
printCata = getConst . C.cata alg
  where
    alg :: C.Alg HGit (Const String)
    alg (Leaf x)       = Const $ "leaf:" ++ x
    alg (Blob xs)  = Const $ "blobNode: [" ++ intercalate ", " (fmap getConst xs) ++ "]"
    alg (File n x) = Const $ "file(" ++ n ++ "): " ++ getConst x
    alg (Dir n xs) = Const $ "dir(" ++ n ++ "): [" ++ intercalate ", " (fmap getConst xs) ++ "]"
    alg (Commit n x) = Const $ "commit(" ++ n ++ "): " ++ getConst x
    alg (Branch n x) = Const $ "branch(" ++ n ++ "):" ++ getConst x

printCata''
  :: forall i
   . C.Term (FC.Compose ((,) HashPointer) :++ HGit) i
  -> String
printCata'' = getConst . C.cata alg
  where
    alg :: C.Alg (FC.Compose ((,) HashPointer) :++ HGit) (Const String)
    alg (HC (FC.Compose (p, Leaf x)))
      = Const $ "leaf[#" ++ show p ++ "]:" ++ x
    alg (HC (FC.Compose (p, Blob xs)))
      = Const $ "blobNode[#" ++ show p ++ "]: [" ++ intercalate ", " (fmap getConst xs) ++ "]"
    alg (HC (FC.Compose (p, File n x)))
      = Const $ "file[#" ++ show p ++ "](" ++ n ++ "): " ++ getConst x
    alg (HC (FC.Compose (p, Dir n xs)))
      = Const $ "dir[#" ++ show p ++ "](" ++ n ++ "): [" ++ intercalate ", " (fmap getConst xs) ++ "]"
    alg (HC (FC.Compose (p, Commit n x)))
      = Const $ "commit[#" ++ show p ++ "](" ++ n ++ "): " ++ getConst x
    alg (HC (FC.Compose (p, Branch n x)))
      = Const $ "branch[#" ++ show p ++ "](" ++ n ++ "): " ++ getConst x

data HashPointer = HashPointer Int deriving Show
type HashIndirect = (,) HashPointer :+ Maybe
type LazyHashTagged m = (,) HashPointer :+ m

lazyDeref
  :: forall i m
   . Monad m
  => HFStore m
  -> HashPointer
  -> C.Term (FC.Compose (LazyHashTagged m) :++ HGit) i
lazyDeref deref = C.futu alg . Const
  where
    alg :: C.CVCoalg (FC.Compose (LazyHashTagged m) :++ HGit)
                     (Const HashPointer)
    alg (Const p) = HC $ FC.Compose $ C (p, C.hfmap helper <$> deref p)


    helper :: C.Term (FC.Compose HashIndirect :++ HGit)
                 :-> C.Context (FC.Compose (LazyHashTagged m) :++ HGit) (Const HashPointer)
    helper (C.Term (HC (FC.Compose (C (p, Nothing))))) = C.Hole $ Const p
    helper (C.Term (HC (FC.Compose (C (p, Just x))))) =
      C.Term $ HC (FC.Compose (C (p, pure $ C.hfmap helper x)))



type HFStore m
   = forall x. HashPointer
  -> m $ HGit (C.Term (FC.Compose HashIndirect :++ HGit)) x

-- | This type represents natural transformations.
type f :--> g = forall (i :: k) . f i -> g i


class MyHFunctor (h :: (k -> *) -> k -> *) where
    -- A higher-order functor @f@ maps every functor @g@ to a
    -- functor @f g@.
    --
    -- @ffmap :: (Functor g) => (a -> b) -> f g a -> f g b@
    --
    -- We omit this, as it does not work for GADTs (see Johand and
    -- Ghani 2008).

    -- | A higher-order functor @f@ also maps a natural transformation
    -- @g :-> h@ to a natural transformation @f g :-> f h@
    myhfmap :: (f :--> g) -> h f :--> h g



data Empty
data NonEmpty

data SafeIntList a b where
  Nil  :: SafeIntList a Empty
  Cons :: Int -> a b -> SafeIntList a NonEmpty

instance C.HFunctor SafeIntList where
  hfmap _ (Nil)      = Nil
  hfmap f (Cons a next) = Cons a $ f next


data EmptyOrNonEmpty = Empty' | NonEmpty'

data SafeIntList' (a :: EmptyOrNonEmpty -> *) b where
  Nil'  :: SafeIntList' a Empty'
  Cons' :: Int -> a EmptyOrNonEmpty -> SafeIntList' a NonEmpty'

-- instance MyHFunctor SafeIntList' where
--   myhfmap _ (Nil')      = Nil'
--   myhfmap f (Cons' a next) = Cons' a $ f next
