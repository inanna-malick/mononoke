module HGit.Types.Merkle where

--------------------------------------------
import           Data.Functor.Const
import qualified Data.Functor.Compose as FC
import           Data.Singletons.TH
--------------------------------------------
import           HGit.Types.Common
import           Util.MyCompose
import           Util.HRecursionSchemes -- YOLO 420 SHINY AND CHROME
--------------------------------------------

$(singletons [d|
  data HGitTag = FileChunkTag | DirTag | CommitTag
  |])

data HGit a i where
  -- file chunk bits
  Blob :: FileChunk -> HGit a 'FileChunkTag
  BlobTree :: [a 'FileChunkTag] -> HGit a 'FileChunkTag

  -- dir and file bits
  File :: PartialFilePath -> a 'FileChunkTag -> HGit a 'DirTag
  Dir :: PartialFilePath -> [a 'DirTag] -> HGit a 'DirTag

  -- commits (what name should root dir have? forest instead of tree?)
  Commit :: CommitMessage
         -> a 'DirTag -- root directory (empty string name for now)
         -> a 'CommitTag -- previous commit
         -> HGit a 'CommitTag
  NullCommit :: HGit a 'CommitTag


instance HFunctor HGit where
  hfmap _ (Blob fc)        = Blob fc
  hfmap f (BlobTree fcs)   = BlobTree $ fmap f fcs
  hfmap f (File n fc)      = File n $ f fc
  hfmap f (Dir n dcs)      = Dir n $ fmap f dcs
  hfmap f (Commit n rc nc) = Commit n (f rc) (f nc)
  hfmap _  NullCommit      = NullCommit


instance HTraversable HGit where
  hmapM _ (Blob fc) = pure $ Blob fc
  hmapM nat (BlobTree fcs) = do
    fcs' <- traverse nat fcs
    pure $ BlobTree fcs'
  hmapM nat (File n fc) = do
    fc' <- nat fc
    pure $ File n fc'
  hmapM nat (Dir n dcs) = do
    dcs' <- traverse nat dcs
    pure $ Dir n dcs'
  hmapM nat (Commit msg rc nc) = do
    rc' <- nat rc
    nc' <- nat nc
    pure $ Commit msg rc' nc'
  hmapM _  NullCommit = pure NullCommit


instance SHFunctor HGit where
  shfmap _ (Blob fc)        = Blob fc
  shfmap f (BlobTree fcs)   = BlobTree $ fmap f fcs
  shfmap f (File n fc)      = File n $ f fc
  shfmap f (Dir n dcs)      = Dir n $ fmap f dcs
  shfmap f (Commit n rc nc) = Commit n (f rc) (f nc)
  shfmap _  NullCommit      = NullCommit

type MyHGit  = Term HGit 'DirTag
type MyBlobTree = Term HGit 'FileChunkTag

-- TODO/NOTE: is this just ctx with hash pointer shaped holes?
--            idk, but can't do recursion schemes over that, can I?
type HashIndirect = (,) HashPointer :+ Maybe
type LazyHashTagged m = (,) HashPointer :+ m

pointer :: forall f i x. Term (FC.Compose ((,) HashPointer :+ x) :++ f) i -> HashPointer
pointer (Term (HC (FC.Compose (C (p, _))))) = p

pointer' :: forall f x . Term (FC.Compose ((,) HashPointer :+ x) :++ f) :-> Const HashPointer
pointer' = Const . pointer

dname :: forall meh. HGit meh 'DirTag -> PartialFilePath
dname (File n _) = n
dname (Dir  n _) = n
