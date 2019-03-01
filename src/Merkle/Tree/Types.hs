{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}



module Merkle.Tree.Types where

--------------------------------------------
import           Data.Functor.Const
import qualified Data.Functor.Compose as FC
import           Data.List (intercalate)
import           Data.Singletons.TH
--------------------------------------------
import           Util.MyCompose
import           Util.HRecursionSchemes -- YOLO 420 SHINY AND CHROME
--------------------------------------------

type Name = String
type FileChunk = String

$(singletons [d|
  data HGitTag = FileChunkTag | DirTag | CommitTag
  |])

data HGit a i where
  -- file chunk bits
  Blob :: FileChunk -> HGit a 'FileChunkTag
  BlobTree :: [a 'FileChunkTag] -> HGit a 'FileChunkTag

  -- dir and file bits
  File :: Name -> a 'FileChunkTag -> HGit a 'DirTag
  Dir :: Name -> [a 'DirTag] -> HGit a 'DirTag

  -- commits
  Commit :: Name -> a 'DirTag -> a 'CommitTag -> HGit a 'CommitTag
  NullCommit :: HGit a 'CommitTag


instance HFunctor HGit where
  hfmap _ (Blob fc)        = Blob fc
  hfmap f (BlobTree fcs)   = BlobTree $ fmap f fcs
  hfmap f (File n fc)      = File n $ f fc
  hfmap f (Dir n dcs)      = Dir n $ fmap f dcs
  hfmap f (Commit n rc nc) = Commit n (f rc) (f nc)
  hfmap _  NullCommit      = NullCommit

instance SHFunctor HGit where
  shfmap _ (Blob fc)        = Blob fc
  shfmap f (BlobTree fcs)   = BlobTree $ fmap f fcs
  shfmap f (File n fc)      = File n $ f fc
  shfmap f (Dir n dcs)      = Dir n $ fmap f dcs
  shfmap f (Commit n rc nc) = Commit n (f rc) (f nc)
  shfmap _  NullCommit      = NullCommit

type MyHGit  = Term HGit 'DirTag
type MyBlobTree = Term HGit 'FileChunkTag

blob :: FileChunk -> MyBlobTree
blob =
  Term . Blob

blobTree :: MyBlobTree
blobTree = Term (BlobTree [blob "foo", blob "bar", blob "baz"])

dir :: [MyHGit] -> MyHGit
dir xs = Term (Dir "mydir" xs)

file :: MyBlobTree -> MyHGit
file x = Term (File "myfile" x)

testDir :: MyHGit
testDir = dir [file blobTree, dir [file blobTree]]

printCata :: forall i. Term HGit i -> String
printCata = getConst . cata alg
  where
    alg :: Alg HGit (Const String)
    alg (Blob x)       = Const $ "blob:" ++ x
    alg (BlobTree xs)  = Const $ "blobTree: [" ++ intercalate ", " (fmap getConst xs) ++ "]"
    alg (File n x) = Const $ "file(" ++ n ++ "): " ++ getConst x
    alg (Dir n xs) = Const $ "dir(" ++ n ++ "): [" ++ intercalate ", " (fmap getConst xs) ++ "]"
    alg (Commit n root prev) = Const $ "commit(" ++ n ++ "): " ++ getConst root ++ " // " ++ getConst prev
    alg (NullCommit) = Const $ "nullcommit"

printCataM :: forall i . Term (FC.Compose IO :++ HGit) i -> IO ()
printCataM = getConst . cata alg
  where
    alg :: Alg (FC.Compose IO :++ HGit) (Const $ IO ())
    alg (HC (FC.Compose m)) = Const $ do
        m' <- m
        case m' of
          (Blob x)       -> putStrLn $ "blob:" ++ x
          (BlobTree xs)  -> do
            putStrLn $ "blobTree: ["
            _ <- traverse getConst xs
            putStrLn "]"
          (File n x) -> do
            putStrLn $ "file(" ++ n ++ "): "
            getConst x
          (Dir n xs) -> do
            putStrLn $ "dir(" ++ n ++ "): ["
            _ <- traverse getConst xs
            pure ()
          (Commit n root prev) -> do
            putStrLn $ "commit(" ++ n ++ "): "
            getConst root
            putStrLn " // "
            getConst prev
          (NullCommit) -> putStrLn "nullcommit"


printCata''
  :: forall i
   . Term (FC.Compose ((,) HashPointer) :++ HGit) i
  -> String
printCata'' = getConst . cata alg
  where
    alg :: Alg (FC.Compose ((,) HashPointer) :++ HGit) (Const String)
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
      = Const $ "nullcommit[" ++ show p ++ "]"

type HashPointer = Int
type HashIndirect = (,) HashPointer :+ Maybe -- note- is this just ctx with hash pointer shaped holes?
type LazyHashTagged m = (,) HashPointer :+ m

pointer :: forall f i x. Term (FC.Compose ((,) HashPointer :+ x) :++ f) i -> HashPointer
pointer (Term (HC (FC.Compose (C (p, _))))) = p

pointer' :: forall f x . Term (FC.Compose ((,) HashPointer :+ x) :++ f) :-> Const HashPointer
pointer' = Const . pointer


dname :: forall meh. HGit meh 'DirTag -> Name
dname (File n _) = n
dname (Dir  n _) = n

type HFStore m
   = forall (x :: HGitTag)
   . SingI x
  => HashPointer
  -> m $ HGit (Term (FC.Compose HashIndirect :++ HGit)) x
