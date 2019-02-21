{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util.Util where -- this module name is a sin against god, todo something descriptive


import           Control.Monad.Except
import           Data.List (intercalate)

import qualified Data.Comp.Multi.Algebra as C
import qualified Data.Comp.Multi.HFunctor as C
import           Data.Comp.Multi.HFunctor ((:->))
import qualified Data.Comp.Multi.Term as C
import           Data.Comp.Multi.Ops ((:&:)(..))

import           Util.MyCompose hiding (getCompose)
import           Data.Functor.Const
import           Data.Functor.Compose


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



printCata' :: forall i. C.Term (DirTree :&: String) i -> String
printCata' = getConst . C.cata alg
  where
    alg :: forall x. (DirTree :&: String) (Const String) x -> Const String x
    alg (Leaf x       :&: s)
      = Const $ "leaf[#" ++ s ++ "]:" ++ x
    alg (BlobNode xs  :&: s)
      = Const $ "blobNode[#" ++ s ++ "]: [" ++ intercalate ", " (fmap getConst xs) ++ "]"
    alg (FileNode n x :&: s)
      = Const $ "file[#" ++ s ++ "](" ++ n ++ "): " ++ getConst x
    alg (DirNode n xs :&: s)
      = Const $ "dir[#" ++ s ++ "](" ++ n ++ "): [" ++ intercalate ", " (fmap getConst xs) ++ "]"


-- printCata''
--   :: forall i
--    . C.Term (Compose ((,) String) :++ DirTree) i
--   -> String
-- printCata'' = getConst . C.cata alg
--   where
--     alg :: forall x. (Compose ((,) String) :++ DirTree) (Const String) x -> Const String x
--     alg (HC (Compose (s, Leaf x)))
--       = Const $ "leaf[#" ++ s ++ "]:" ++ x
--     alg (HC (Compose (s, BlobNode xs)))
--       = Const $ "blobNode[#" ++ s ++ "]: [" ++ intercalate ", " (fmap getConst xs) ++ "]"
    -- alg (FileNode n x :&: s)
    --   = Const $ "file[#" ++ s ++ "](" ++ n ++ "): " ++ getConst x
    -- alg (DirNode n xs :&: s)
    --   = Const $ "dir[#" ++ s ++ "](" ++ n ++ "): [" ++ intercalate ", " (fmap getConst xs) ++ "]"

-- myFunction
--   :: forall i f f' g
--    . Functor f
--   => C.HFunctor g
--   => (forall x. f x -> f' x)
--   -> C.Term (Compose f  :++ g) i
--   -> C.Term (Compose f' :++ g) i
-- myFunction nat = getConst . C.cata alg
--   where
--     alg :: forall i' . C.Alg (Compose f :++ g) (Const (C.Term (Compose f' :++ g) i'))
--     alg (HC (Compose fg))
--       = Const . C.Term . HC . Compose $ nat fg
--       -- = Const . C.Term . HC . Compose $ fmap (C.hfmap getConst) $ nat fg


myFunction
  :: forall i f f' g
   . Functor f
  => C.HFunctor g
  => f :-> f'
  -> C.Term (Compose f  :++ g) i
  -> C.Term (Compose f' :++ g) i
myFunction nat x
  = let fg = fmap (C.hfmap (myFunction nat)) $ getCompose $ getHCompose $ C.unTerm x
     in C.Term . HC . Compose $ nat fg
