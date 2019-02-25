{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}



module Merkle.Tree.Types where

--------------------------------------------
import           Data.Functor.Foldable
--------------------------------------------
import           Merkle.Types (WithHash)
import           Util.MyCompose
import qualified Util.HRecursionSchemes as C -- YOLO 420 SHINY AND CHROME
import           Util.HRecursionSchemes ((:->)) -- YOLO 420 SHINY AND CHROME
--------------------------------------------

import           Data.Singletons.TH
import           Data.Singletons


import           Data.Kind (Type)

import           Control.Monad.Except
import           Data.List (intercalate)

-- import qualified Data.Comp.Multi.Algebra as C
-- import qualified Data.Comp.Multi.HFunctor as C
-- import           Data.Comp.Multi.HFunctor ((:->))
-- import qualified Data.Comp.Multi.Term as C

import           Util.MyCompose
import           Data.Functor.Const
import qualified Data.Functor.Compose as FC

-- -- | Tree in which leaf nodes are specialized to String
-- data Tree a = Node [a] | Leaf String deriving (Eq, Show, Functor, Foldable, Traversable)


-- type Name = String

-- -- | Entity tagged with a name
-- type Named = (,) Name

-- -- | merkle tree that at any level (including the top) can either consist of
-- --   hash-addressed pointers to nodes or substantiated named tree nodes paired with their pointer
-- type LazyMerkleTree = Fix (WithHash :+ Maybe :+ Named :+ Tree)

-- -- | merkle tree that has all nodes fully substantiated
-- type StrictMerkleTree = Fix (WithHash :+ Named :+ Tree)



type Name = String
type FileChunk = String




$(singletons [d|
  data HGitTag = FileChunkTag | DirTag | CommitTag | MetaTag
  |])


-- data HGitTag = FileChunkTag | DirTag | CommitTag | MetaTag
-- data SingHGT :: HGitTag -> Type where
--     SFileChunkTag :: SingHGT 'FileChunkTag
--     SDirTag :: SingHGT 'DirTag
--     SCommitTag :: SingHGT 'CommitTag
--     SMetaTag :: SingHGT 'MetaTag

-- class SingHGTI s where
--     singHGT :: SingHGT s

-- instance SingHGTI 'FileChunkTag where singHGT = SFileChunkTag
-- instance SingHGTI 'DirTag where singHGT = SDirTag
-- instance SingHGTI 'CommitTag where singHGT = SCommitTag
-- instance SingHGTI 'MetaTag where singHGT = SMetaTag

magic :: forall (x :: HGitTag) . SingI x => Sing x
magic = undefined

data HGit a i where
  -- file chunk bits
  Leaf :: FileChunk -> HGit a FileChunkTag
  Blob :: [a FileChunkTag] -> HGit a FileChunkTag

  -- dir and file bits
  File :: Name -> a FileChunkTag -> HGit a DirTag
  Dir :: Name -> [a DirTag] -> HGit a DirTag

  -- commits
  Commit :: Name -> a DirTag -> Maybe (a CommitTag) -> HGit a CommitTag

  -- branches
  Branch :: Name -> a CommitTag -> HGit a MetaTag


instance C.HFunctor HGit where
  hfmap _ (Leaf fc)      = Leaf fc
  hfmap f (Blob fcs)     = Blob $ fmap f fcs
  hfmap f (File n fc)    = File n $ f fc
  hfmap f (Dir n dcs)    = Dir n $ fmap f dcs
  hfmap f (Commit n cc mcc)  = Commit n (f cc) (fmap f mcc)
  hfmap f (Branch n bc)  = Branch n $ f bc

instance C.SHFunctor HGit where
  shfmap _ (Leaf fc)      = Leaf fc
  shfmap f (Blob fcs)     = Blob $ fmap f fcs
  shfmap f (File n fc)    = File n $ f fc
  shfmap f (Dir n dcs)    = Dir n $ fmap f dcs
  shfmap f (Commit n cc mcc)  = Commit n (f cc) (fmap f mcc)
  shfmap f (Branch n bc)  = Branch n $ f bc

type MyHGit  = C.Term HGit DirTag
type MyBlobTree = C.Term HGit FileChunkTag

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
    alg (Commit n x xs) = Const $ "commit(" ++ n ++ "): " ++ getConst x ++ " // " ++ show (fmap getConst xs)
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
    alg (HC (FC.Compose (p, Commit n x mx)))
      = Const $ "commit[#" ++ show p ++ "](" ++ n ++ "): " ++ getConst x ++" // " ++ show (fmap getConst mx)
    alg (HC (FC.Compose (p, Branch n x)))
      = Const $ "branch[#" ++ show p ++ "](" ++ n ++ "): " ++ getConst x

data HashPointer = HashPointer Int deriving Show
type HashIndirect = (,) HashPointer :+ Maybe -- note- is this just ctx with hash pointer shaped holes?
type LazyHashTagged m = (,) HashPointer :+ m


type HFStore m
   = forall (x :: HGitTag)
   . SingI x
  => HashPointer
  -> m $ HGit (C.Term (FC.Compose HashIndirect :++ HGit)) x


lazyDeref
  :: forall i m
   . Monad m
  => SingI i
  => HFStore m
  -> HashPointer
  -> C.Term (FC.Compose (LazyHashTagged m) :++ HGit) i
lazyDeref deref = C.sFutu alg . Const
  where
    alg :: C.SCVCoalg
             (FC.Compose (LazyHashTagged m) :++ HGit)
             (Const HashPointer)
    alg (Const p) = HC $ FC.Compose $ C (p, C.hfmap helper <$> deref p)


    helper :: C.Term (FC.Compose HashIndirect :++ HGit)
                 :-> C.Context (FC.Compose (LazyHashTagged m) :++ HGit) (Const HashPointer)
    helper (C.Term (HC (FC.Compose (C (p, Nothing))))) = C.Hole $ Const p
    helper (C.Term (HC (FC.Compose (C (p, Just x))))) =
      C.Term $ HC (FC.Compose (C (p, pure $ C.hfmap helper x)))
