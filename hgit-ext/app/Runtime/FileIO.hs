-- | Functions for interacting with the filesystem to
-- create dir trees from merkle trees or vice versa
module Runtime.FileIO where

--------------------------------------------
import           Control.Monad.Except
import           Control.Monad.Trans.State.Lazy
import           Data.Bitraversable (bitraverse)
import qualified Data.List as List
import           Data.Foldable (traverse_)
import qualified System.Directory as Dir
--------------------------------------------
import           Util.RecursionSchemes
import           HGit.Core.Types
--------------------------------------------


-- | Write strict hgit dirtree to file path
writeTree
  :: forall m
   . MonadIO m
  => FilePath
  -> Fix (Dir (Fix Blob))
  -> m ()
writeTree outdir tree = evalStateT (cataM writeDir tree) [outdir]
  where
    writeFileChunk :: AlgebraM (StateT [FilePath] m) Blob ()
    writeFileChunk (ChunkList _) = pure () -- no work here
    writeFileChunk (Chunk contents) = -- NOTE: should probably confirm this is an in-order traversal, but IIRC it is
      gets (List.intercalate "/" . reverse) >>=  liftIO . flip appendFile contents

    writeDir :: AlgebraM (StateT [FilePath] m) (Dir (Fix Blob)) ()
    writeDir (Dir children) = flip traverse_ children $ \(pathChunk, e) -> do
      modify (push pathChunk)
      _ <- bitraverse (\fb -> touch >> cataM writeFileChunk fb) -- todo: error if file already exists
                      (\() -> mkDir) e
      modify pop

    mkDir = gets (List.intercalate "/" . reverse) >>= liftIO . Dir.createDirectory
    touch = gets (List.intercalate "/" . reverse) >>= liftIO . flip writeFile ""

    push x xs = x:xs
    pop (_:xs)  = xs
    pop []    = []



readTree
  :: FilePath
  -> IO (Fix (Dir (Fix Blob)))
readTree = anaM alg
  where
    alg :: CoAlgebraM IO (Dir (Fix Blob)) FilePath
    alg path = do
          putStrLn $ "read dir at: " ++ path
          dirContents <- Dir.getDirectoryContents path
          let dirContents'
                = fmap (\x -> path ++ "/" ++ x)
                -- ignore all starting with '.' (eg ..,.,.hgit/)
                . filter (\fn -> take 1 fn /= ".")
                $ dirContents
          dirContents'' <- traverse categorize dirContents'
          pure $ Dir $ fmap (\(p,e) -> (justTheName p, e)) dirContents''

    categorize p = do
      isFile <- Dir.doesFileExist p
      if isFile
        then (justTheName p,) . FileEntity <$> readBlob p
        -- then (justTheName p,) . FileEntity . Fix . (\x -> Chunk x (Fix Empty)) <$> readFile p
        else do
          isDir <- Dir.doesDirectoryExist p
          if isDir
            then pure $ (justTheName p, DirEntity p)
            else fail ("file read error: unexpected type at " ++ p)


justTheName :: FilePath -> String -- hacky hax but it works - take just the name given a file path
justTheName = reverse . takeWhile (/= '/') . reverse


readBlob
  :: FilePath
  -> IO (Fix Blob)
readBlob p = do
    b <- readFile p
    pure . Fix
         . ChunkList
         $ fmap (Fix . Chunk . unlines)
                (breakLines blobSizeInLines $ lines b)

  where
    -- totally arbitrary and artificial, chosen based on what looks good when rendering
    -- FIXME: totally screws up performance as compared to just reading bytestrings, but
    --        this is a demo so that's low priority for now. What's high priority? you
    --        guessed it, having blobs that render nicely when one is browsing raw files
    --        in talk/demo context. Punting on dealing w/ unicode chars broken by blob
    --        boundaries comma lmao FIXME FIXME FIXME
    -- NOTE: this makes my partner groan when I describe it to them and is just, like
    --       terrible. so terrible. Please fix when it seems fun/interesting to do so
    blobSizeInLines = 32

    breakLines _n [] = []
    breakLines n xs  = take n xs : breakLines n (drop n xs)
