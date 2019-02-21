{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util.Util where -- this module name is a sin against god, todo something descriptive


import           Control.Monad.Except
import           Data.List (intercalate)

import qualified Data.Comp.Multi.Algebra as C
import qualified Data.Comp.Multi.HFunctor as C
import qualified Data.Comp.Multi.Term as C

import           Data.Functor.Const


mapErrUtil :: Functor m => (e -> e') -> ExceptT e m a -> ExceptT e' m a
mapErrUtil f = mapExceptT (fmap (either (Left . f) Right))

type Name = String
type FileChunk = String

data DirTree a i where
  -- file chunk bits
  Leaf :: FileChunk -> DirTree a FileChunk
  BlobNode :: [a FileChunk] -> DirTree a FileChunk

  -- dir and file bits
  FileNode :: Name -> a FileChunk -> DirTree a ()
  DirNode :: Name -> [a ()] -> DirTree a ()

instance C.HFunctor DirTree where
  hfmap _ (Leaf fc)        = Leaf fc
  hfmap f (BlobNode fcs)   = BlobNode $ fmap f fcs
  hfmap f (FileNode n fnc) = FileNode n $ f fnc
  hfmap f (DirNode n dncs)  = DirNode n $ fmap f dncs

type MyDirTree  = C.Term DirTree ()
type MyBlobTree = C.Term DirTree FileChunk

leaf :: FileChunk -> MyBlobTree
leaf = C.Term . Leaf

blobTree :: MyBlobTree
blobTree = C.Term (BlobNode [leaf "foo", leaf "bar", leaf "baz"])

dir :: [MyDirTree] -> MyDirTree
dir xs = C.Term (DirNode "mydir" xs)

file :: MyBlobTree -> MyDirTree
file x = C.Term (FileNode "myfile" x)

testDir :: MyDirTree
testDir = dir [file blobTree, dir [file blobTree]]

printCata :: forall i. C.Term DirTree i -> String
printCata = getConst . C.cata alg
  where
    -- NOTE: feels like x should be i?
    alg :: forall x. DirTree (Const String) x -> Const String x
    alg (Leaf x)       = Const $ "leaf:" ++ x
    alg (BlobNode xs)  = Const $ "blobNode: [" ++ intercalate ", " (fmap getConst xs) ++ "]"
    alg (FileNode n x) = Const $ "file(" ++ n ++ "): " ++ getConst x
    alg (DirNode n xs) = Const $ "dir(" ++ n ++ "): [" ++ intercalate ", " (fmap getConst xs) ++ "]"
