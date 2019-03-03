-- | Functions for interacting with the filesystem to
-- create dir trees from merkle trees or vice versa
module FileIO where -- (readTree, writeTree, writeTree') where

--------------------------------------------
import           Control.Monad.Except
import           Control.Monad.Trans.State.Lazy
import qualified Data.List as List
import           Data.Foldable (traverse_)
import qualified System.Directory as Dir
--------------------------------------------
import           Util.MyCompose
import           Util.HRecursionSchemes
import           HGit.Types
--------------------------------------------
import qualified Data.Functor.Compose as FC
import           Data.Functor.Const
import           Data.Singletons

-- | Write strict hgit dirtree to file path
writeTree
  :: MonadIO m
  => FilePath
  -> Term HGit 'DirTag
  -> m ()
writeTree outdir tree = do
  liftIO $ evalStateT (getConst $ sCata alg tree) [outdir]

  where
    alg :: SAlg HGit (Const $ StateT [FilePath] IO ())
    alg (Blob contents)    = Const $ do -- append - will open file handle multiple times, w/e, can cache via state later
      path <- List.intercalate "/" . reverse <$> get
      liftIO $ appendFile path contents

    alg (BlobTree children) = Const $ traverse_ getConst children

    alg (Dir children) = Const $ do -- mkdir
      let mkDir = do
            path <- List.intercalate "/" . reverse <$> get
            liftIO $ Dir.createDirectory path
          touch = do
            path <- List.intercalate "/" . reverse <$> get
            liftIO $ writeFile path "" -- touch file
          handle (pathChunk, e) = do
            modify (push pathChunk)
            either (\_ -> mkDir) (\_ -> touch) e
            either getConst getConst e
            modify pop
      traverse_ handle children

    -- unreachable
    alg _ = undefined

    push x xs = x:xs
    pop (_:xs)  = xs
    pop []    = []

-- | Lazily read some directory tree into memory
-- NOTE: this needs to be a futu so file steps can read the full file into memory instead of recursing into filepointers
readTree
  :: forall m
   . MonadIO m
  => FilePath
  -- tree structure _without_ pointer annotation
  -- type-level guarantee that there is no hash identified
  -- entity indirection allowed here
  -> Term (FC.Compose m :++ HGit) 'DirTag
readTree = sFutu alg . Const
  where
    alg :: SCVCoalg (FC.Compose m :++ HGit) (Const FilePath)
    alg (Const path) = readTree' sing path

readTree'
  :: forall x m . MonadIO m => Sing x -> FilePath
  -> (FC.Compose m :++ HGit) (Cxt Hole (FC.Compose m :++ HGit) (Const FilePath)) x
readTree' s path = HC $ FC.Compose $ case s of
      SFileChunkTag -> liftIO $ fmap Blob $ readFile path
      SDirTag -> do
          dirContents  <- liftIO $ Dir.getDirectoryContents path
          let dirContents'
                = filter (/= ".")
                . filter (/= "..")
                $ dirContents
          dirContents'' <- traverse categorize dirContents'
          pure $ Dir dirContents''

      -- unreachable
      _ -> fail ("quote 'unreachable' unquote")

  where
    categorize path = do
      isFile <- liftIO $ Dir.doesFileExist path
      if isFile
        then pure $ (justTheName path, Right $ Hole $ Const path)
        else do
          isDir <- liftIO $ Dir.doesDirectoryExist path
          if isDir
            then pure $ (justTheName path, Left $ Hole $ Const path)
            else fail ("file read error: unexpected type at " ++ path)


justTheName :: FilePath -> String -- hacky hax but it works - take just the name given a file path
justTheName = reverse . takeWhile (/= '/') . reverse
