module Main where

--------------------------------------------
import           Data.Functor.Compose
import qualified Data.List as L
import           Data.List.NonEmpty
import qualified Data.Map as M
import qualified System.Directory as Dir
--------------------------------------------
import           Commands
import           FileIO
import           HGit.Diff (diffMerkleDirs)
import           HGit.Repo
import           HGit.Types
import           HGit.Merge
import           Util.RecursionSchemes
import           Merkle.Functors
import           Merkle.Store
import           Merkle.Store.Deref
import           Merkle.Types
--------------------------------------------

main :: IO ()
main = parse >>= \case
  InitRepo -> do
    mkHgitDir -- mk .hgit and .hgit/store dirs
    writeState initialRepoState

  CheckoutBranch branch _ -> do
    dstore        <- mkStore "dirs"
    bstore        <- mkStore "blobs"
    cstore        <- mkStore "commits"
    base         <- baseDir
    repostate    <- readState

    targetCommit <- getBranch branch repostate >>= sDeref cstore

    diffs        <- status base repostate cstore dstore -- todo drop all these into a struct
    if not (null diffs)
      then do
        putStrLn "directory modified, cannot checkout. Changes:"
        _ <- traverse renderDiff diffs
        fail "womp womp"
      else do
        currentCommit <- getBranch (currentBranch repostate) repostate >>= sDeref cstore
        topLevelCurrentDir <- sDeref dstore $ commitRoot currentCommit

        setDirTo dstore bstore base topLevelCurrentDir (commitRoot targetCommit)
        writeState $ repostate
                  { currentBranch = branch
                  }

  MkBranch branch -> do
    repostate <- readState
    current   <- getBranch (currentBranch repostate) repostate
    writeState $ repostate
               { branches      = M.insert branch current $ branches repostate
               , currentBranch = branch
               }

  MkCommit msg -> do
    bstore            <- mkStore "blobs"
    cstore            <- mkStore "commits"
    dstore            <- mkStore "dirs"
    repostate         <- readState
    base              <- baseDir
    currentCommitHash <- getBranch (currentBranch repostate) repostate

    let uploadBlobs :: Fix (Dir (Fix Blob)) -> IO (Fix (Dir (Hash Blob)))
        uploadBlobs = cataM (\(Dir xs) -> fmap (Fix . Dir) $
                               traverse (traverse (fte (fmap FileEntity . uploadDeep bstore) (pure . DirEntity))) xs
                            )

    currentStateHash <- readTree base >>= uploadBlobs >>= uploadDeep dstore

    let commit = Commit msg currentStateHash (pure currentCommitHash)
    rootHash <- sUploadShallow cstore commit
    writeState $ repostate
               { branches = M.insert (currentBranch repostate) rootHash $ branches repostate
               }

  -- todo: n-way branch merge once I figure out UX
  MkMergeCommit targetBranch msg -> do
    cstore            <- mkStore "commits"
    dstore            <- mkStore "dirs"
    bstore            <- mkStore "blobs"
    repostate         <- readState
    base              <- baseDir

    diffs        <- status base repostate cstore dstore
    if not (null diffs)
      then do
        putStrLn "directory modified, cannot merge. Changes:"
        _ <- traverse renderDiff diffs
        fail "womp womp"
      else do

        targetCommitHash  <- getBranch targetBranch repostate
        currentCommitHash <- getBranch (currentBranch repostate) repostate

        currentCommit <- sDeref cstore currentCommitHash
        targetCommit <- sDeref cstore targetCommitHash

        mergeRes <- mergeMerkleDirs dstore (commitRoot currentCommit) (commitRoot targetCommit)

        case mergeRes of
          Left err -> fail $ "merge nonviable due to: " ++ show err
          Right root -> do
            let commit = Commit msg (htPointer root) $ currentCommitHash :| [targetCommitHash]
            rootHash <- sUploadShallow cstore commit
            writeState $ repostate
                       { branches = M.insert (currentBranch repostate) rootHash
                                  $ branches repostate
                       }


            topLevelCurrentDir <- sDeref dstore $ commitRoot currentCommit
            setDirTo dstore bstore base topLevelCurrentDir $ htPointer root

  GetStatus -> do
    cstore    <- mkStore "commits"
    dstore    <- mkStore "dirs"
    repostate <- readState
    base      <- baseDir
    diffs     <- status base repostate cstore dstore

    putStrLn $ "current branch: " ++ currentBranch repostate
    putStrLn $ "diffs:"
    _ <- traverse renderDiff diffs
    pure ()

  GetDiff branch1 branch2 -> do
    cstore    <- mkStore "commits"
    dstore    <- mkStore "dirs"
    repostate <- readState
    commit1   <- getBranch branch1 repostate >>= sDeref cstore
    commit2   <- getBranch branch2 repostate >>= sDeref cstore
    diffs     <- diffMerkleDirs (lazyDeref dstore $ commitRoot commit1)
                                (lazyDeref dstore $ commitRoot commit2)
    _ <- traverse renderDiff diffs
    pure ()

  where
    renderDiff (fps, d) = putStrLn $ "\t" ++ show d ++ " at " ++ (L.intercalate "/" fps)

    status base repostate cstore dstore = do
      currentCommit <- getBranch (currentBranch repostate) repostate >>= sDeref cstore
      strictCurrentState  <- readTree base

      -- TODO: beautify
      let currentState :: Fix (HashTagged (Dir (Hash Blob)) `Compose` IO `Compose` Dir (Hash Blob))
          currentState
            = cata (\(Dir xs) ->
                      let xs' = fmap (fmap (ftem (cata hash) id)) xs
                       in Fix $ Compose (hash $ Dir $ fmap (fmap (ftem id htPointer)) xs', Compose $ pure $ Dir xs')
                   ) strictCurrentState
      diffMerkleDirs (lazyDeref dstore $ commitRoot currentCommit) currentState



    -- x, y, don't matter..
    setDirTo :: forall x y. Store IO (Dir (Hash Blob)) -> Store IO Blob
             -> FilePath -> Dir x y -> Hash (Dir (Hash Blob))
             -> IO ()
    setDirTo dstore bstore base topLevelCurrentDir targetDir = do
      let toDelete = dirEntries topLevelCurrentDir

      -- NOTE: basically only use in a docker container for a bit, lol
      -- delete each top-level entity in the current commit's root dir
      -- we just confirmed that there are no diffs btween it and the current dir state
      let cleanup (p, DirEntity  _) = Dir.removeDirectoryRecursive p
          cleanup (p, FileEntity _) = Dir.removeFile p
      _ <- traverse cleanup toDelete

      (x :: Fix (Dir (Hash Blob))) <- fmap stripTags $ strictDeref $ lazyDeref dstore targetDir

      let f :: forall x' m' y' . (x' -> m' y') -> Fix (Dir x') -> m' (Fix (Dir y'))
          f = undefined

      (x' :: Fix (Dir (Fix Blob))) <- f (fmap stripTags . strictDeref . lazyDeref bstore) x

      writeTree base x'


commitRoot
  :: Commit (Hash (Dir (Hash Blob))) b
  -> Hash (Dir (Hash Blob))
commitRoot (Commit _ x _) = x
commitRoot NullCommit     = emptyHash
