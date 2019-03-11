-- | Functions for interacting with the filesystem to
-- create dir trees from merkle trees or vice versa
module FileIO where -- (readTree, writeTree, writeTree') where

--------------------------------------------
import           Control.Monad.Except
import           Control.Monad.Trans.State.Lazy
import qualified Data.List as List
import           Data.Foldable (traverse_)
import qualified Data.Functor.Compose as FC
import           Data.Singletons
import qualified System.Directory as Dir
--------------------------------------------
import           Util.MyCompose
import           Util.HRecursionSchemes
import           HGit.Types
import           Merkle.Store
import           Merkle.Types (HashPointer)
--------------------------------------------

-- | Write strict hgit dirtree to file path
writeTree
  :: forall m
   . MonadIO m
  => FilePath
  -> Term HGit 'DirTag
  -> m ()
writeTree outdir tree = evalStateT (writeDir tree) [outdir]
  where
    writeFileChunk :: Term HGit 'BlobTag -> StateT [FilePath] m ()
    -- append - will open file handle multiple times, w/e, can cache via state later
    writeFileChunk (Term (Blob contents)) =
      gets (List.intercalate "/" . reverse) >>=  liftIO . flip appendFile contents
    writeDir :: Term HGit 'DirTag -> StateT [FilePath] m ()
    writeDir (Term (Dir children)) = flip traverse_ children $ \(pathChunk, e) -> do
      modify (push pathChunk)
      fte (\(_ :: Term HGit 'BlobTag) -> touch)
          (\(_ :: Term HGit 'DirTag) -> mkDir) e
      fte writeFileChunk writeDir e
      modify pop


    mkDir = gets (List.intercalate "/" . reverse) >>= liftIO . Dir.createDirectory
    touch = gets (List.intercalate "/" . reverse) >>= liftIO . flip writeFile ""

    push x xs = x:xs
    pop (_:xs)  = xs
    pop []    = []


readAndStore
  :: forall m
   . MonadIO m
  => Store m HGit
  -> FilePath
  -> m (Const HashPointer 'DirTag)
readAndStore store = fmap Const . getConst . cata alg . readTree
  where
    alg :: Alg (FC.Compose m :++ HGit) (Const (m HashPointer))
    alg (HC (FC.Compose eff)) = Const $ do
      e <- eff
      -- upload sub-trees and get hashes
      let f :: forall i . HGit (Const (m HashPointer)) i -> m (HGit (Const (HashPointer)) i)
          f = \case
                Blob x -> pure $ Blob x
                Dir xs -> do
                  xs' <- traverse (\(n, et) ->
                                    (n,) <$> fte (fmap (FileEntity . Const) . getConst)
                                                 (fmap (DirEntity . Const) . getConst)
                                                  et
                                  ) xs
                  pure $ Dir xs'
                Commit msg a b -> do
                  a' <- getConst a
                  b' <- traverse getConst b
                  pure $ Commit msg (Const a') (fmap Const b')
                NullCommit ->
                  pure $ NullCommit
      e' <- f e

      -- hax (todo chain through)
      fmap getConst $ sUploadShallow store e'

-- | Lazily read some directory tree into memory
-- NOTE: this needs to be a futu so file steps can read the full file into memory instead of recursing into filepointers (note 2: is this till true? idk lol)
readTree
  :: forall m
   . MonadIO m
  => FilePath
  -- tree structure _without_ pointer annotation
  -- type-level guarantee that there is no hash identified
  -- entity indirection allowed here
  -> Term (FC.Compose m :++ HGit) 'DirTag
readTree = futu alg . Const
  where
    alg :: CVCoalg (FC.Compose m :++ HGit) (Const FilePath)
    alg (Const path) = readTree' sing path

readTree'
  :: forall x m . MonadIO m => Sing x -> FilePath
  -> (FC.Compose m :++ HGit) (Cxt Hole (FC.Compose m :++ HGit) (Const FilePath)) x
readTree' s path = HC $ FC.Compose $ case s of
      SBlobTag -> liftIO $ fmap Blob $ readFile path
      SDirTag -> do
          -- liftIO $ putStrLn $ "readTree' path: " ++ path
          dirContents  <- liftIO $ Dir.getDirectoryContents path
          let dirContents'
                = fmap (\x -> path ++ "/" ++ x)
                -- ignore all starting with '.' (eg ..,.,.hgit/)
                . filter (\fn -> take 1 fn /= ".")
                $ dirContents
          dirContents'' <- traverse categorize dirContents'
          pure $ Dir $ fmap (\(p,e) -> (justTheName p, e)) dirContents''

      -- unreachable
      _ -> fail ("quote 'unreachable' unquote")

  where
    categorize p = do
      isFile <- liftIO $ Dir.doesFileExist p
      if isFile
        then pure $ (justTheName p, FileEntity . Hole $ Const p)
        else do
          isDir <- liftIO $ Dir.doesDirectoryExist p
          if isDir
            then pure $ (justTheName p, DirEntity . Hole $ Const p)
            else fail ("file read error: unexpected type at " ++ p)


justTheName :: FilePath -> String -- hacky hax but it works - take just the name given a file path
justTheName = reverse . takeWhile (/= '/') . reverse
