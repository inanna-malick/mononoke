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
import           Util.MyCompose
import qualified Util.HRecursionSchemes as C -- YOLO 420 SHINY AND CHROME
import           Util.HRecursionSchemes ((:->)) -- YOLO 420 SHINY AND CHROME
--------------------------------------------
import           Data.Singletons.TH
import           Data.Singletons
import           Data.Kind (Type)
import           Control.Monad.Except
import           Data.List (intercalate)
import           Util.MyCompose
import           Data.Functor.Const
import qualified Data.Functor.Compose as FC

type Name = String
type FileChunk = String

$(singletons [d|
  data HGitTag = FileChunkTag | DirTag | CommitTag | MetaTag
  |])

data HGit a i where
  -- file chunk bits
  Blob :: FileChunk -> HGit a FileChunkTag
  BlobTree :: [a FileChunkTag] -> HGit a FileChunkTag

  -- dir and file bits
  File :: Name -> a FileChunkTag -> HGit a DirTag
  Dir :: Name -> [a DirTag] -> HGit a DirTag

  -- commits
  Commit :: Name -> a DirTag -> a CommitTag -> HGit a CommitTag
  NullCommit :: HGit a CommitTag

  -- branches
  Branch :: Name -> a CommitTag -> HGit a MetaTag


instance C.HFunctor HGit where
  hfmap _ (Blob fc)        = Blob fc
  hfmap f (BlobTree fcs)   = BlobTree $ fmap f fcs
  hfmap f (File n fc)      = File n $ f fc
  hfmap f (Dir n dcs)      = Dir n $ fmap f dcs
  hfmap f (Commit n rc nc) = Commit n (f rc) (f nc)
  hfmap f  NullCommit      = NullCommit
  hfmap f (Branch n bc)    = Branch n $ f bc

instance C.SHFunctor HGit where
  shfmap _ (Blob fc)        = Blob fc
  shfmap f (BlobTree fcs)   = BlobTree $ fmap f fcs
  shfmap f (File n fc)      = File n $ f fc
  shfmap f (Dir n dcs)      = Dir n $ fmap f dcs
  shfmap f (Commit n rc nc) = Commit n (f rc) (f nc)
  shfmap f  NullCommit      = NullCommit
  shfmap f (Branch n bc)    = Branch n $ f bc

type MyHGit  = C.Term HGit DirTag
type MyBlobTree = C.Term HGit FileChunkTag

blob :: FileChunk -> MyBlobTree
blob =
  C.Term . Blob

blobTree :: MyBlobTree
blobTree = C.Term (BlobTree [blob "foo", blob "bar", blob "baz"])

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
    alg (Blob x)       = Const $ "blob:" ++ x
    alg (BlobTree xs)  = Const $ "blobTree: [" ++ intercalate ", " (fmap getConst xs) ++ "]"
    alg (File n x) = Const $ "file(" ++ n ++ "): " ++ getConst x
    alg (Dir n xs) = Const $ "dir(" ++ n ++ "): [" ++ intercalate ", " (fmap getConst xs) ++ "]"
    alg (Commit n root prev) = Const $ "commit(" ++ n ++ "): " ++ getConst root ++ " // " ++ getConst prev
    alg (NullCommit) = Const $ "nullcommit"
    alg (Branch n x) = Const $ "branch(" ++ n ++ "):" ++ getConst x

printCata''
  :: forall i
   . C.Term (FC.Compose ((,) HashPointer) :++ HGit) i
  -> String
printCata'' = getConst . C.cata alg
  where
    alg :: C.Alg (FC.Compose ((,) HashPointer) :++ HGit) (Const String)
    alg (HC (FC.Compose (p, Blob x)))
      = Const $ "blob[#" ++ show p ++ "]:" ++ x
    alg (HC (FC.Compose (p, BlobTree xs)))
      = Const $ "blobNode[#" ++ show p ++ "]: [" ++ intercalate ", " (fmap getConst xs) ++ "]"
    alg (HC (FC.Compose (p, File n x)))
      = Const $ "file[#" ++ show p ++ "](" ++ n ++ "): " ++ getConst x
    alg (HC (FC.Compose (p, Dir n xs)))
      = Const $ "dir[#" ++ show p ++ "](" ++ n ++ "): [" ++ intercalate ", " (fmap getConst xs) ++ "]"
    alg (HC (FC.Compose (p, Commit n root prev)))
      = Const $ "commit[#" ++ show p ++ "](" ++ n ++ "): " ++ getConst root ++" // " ++ getConst prev
    alg (HC (FC.Compose (p, NullCommit)))
      = Const $ "nullcommit"
    alg (HC (FC.Compose (p, Branch n x)))
      = Const $ "branch[#" ++ show p ++ "](" ++ n ++ "): " ++ getConst x

type HashPointer = Int
type HashIndirect = (,) HashPointer :+ Maybe -- note- is this just ctx with hash pointer shaped holes?
type LazyHashTagged m = (,) HashPointer :+ m


type HFStore m
   = forall (x :: HGitTag)
   . SingI x
  => HashPointer
  -> m $ HGit (C.Term (FC.Compose HashIndirect :++ HGit)) x
