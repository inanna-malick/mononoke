module Main where

--------------------------------------------
import           Data.Functor.Const
--------------------------------------------
import           Commands
import           FileIO
import           HGit.Diff (diffMerkleDirs)
import           HGit.Repo
import           HGit.Types
import           Util.MyCompose
import           Util.HRecursionSchemes
import           HGit.Serialization (emptyDirHash)
import           HGit.Store
import           HGit.Store.Deref
import           HGit.Store.FileSystem
--------------------------------------------
import qualified Data.Functor.Compose as FC

-- TODO: new app plan - minimum required for cool demo, basically - idea is diffing branches, checking them out, etc
-- checkout: reset current directory to branch - only if no changes (determined by reading current dir and doing diff)
-- idea: --lazy flag, just touches all files but only grabs those you request
--       note: this kinda breaks diffing against current directory, BUT I can have my own format:
--             [filename].hgit.lazy.file OR [dirname].hgit.lazy.dir
--             could then just disallow this postfix on ingestion to avoid possibility of overlap
--             anyway, then just have that be read as diff of subtree and include lazyness in file
--             read... fuck, this is nontrivial. I will do this as part of _V2_ - seriously, nontrivial..
--             that said, it's a hecking good idea.. w/o it no real need for the search path thing either tbh..
--             ok, no worries - branch/checkout/etc is PERFECT for demo v1 thingy, next can be w/e lol
-- idea: that then requires checkout w/ file path (would checkout file and all subdirs and mk same)
--       could just have optional 'only this path if it exists' string and run off that
--       type idea: IO $ Either FileDoesntExistError $ IO ()
--       can then build up actions _but_ only run them (eg intermediate mkdir calls)
--       if no named file is missing
--       this allows for tree traversal and not just single file, have input be list of file parts
--       and use state (as elsewhere) to manage stack - can use * at any level to select all files or dirs and run next thing in list, if * is end of list is treated as Nothing (match all)
--       note: --lazy and --match can be applied to the same traversal via the same code


-- NOTE: hgit is a nice pun name (unintentional), includes features of both hg and git comma lmao

-- NOTE: might as well just keep popping up the directory tree to find that hgit file w/ branch mappings anyway, lol - (or .hgit/branches, with .hgit/store as the store (removes need for flag))

-- requires: xyz
-- new work: .git file and store (oh hey, I can make the store directory part of the 'repo' and do everything at that level) -let's say it's also aeson of this data structure:
 -- todo import and use map? or just object w/ mappings via alias?
-- eg: data RepoData = RepoData [(String, HashPointer)] (string to int map, tolerable usage of json)
-- this is essentially the hg approach

-- NOTE NOTE NOTE
-- can replicate distributed nature of git pretty handily by just having a foreign store send
-- over list of branch names and pulling from said store via http or w/e
-- that's.. pretty easy, and I think would probably help this be a v. compelling demo

-- NOTE NOTE NOTE
-- for status: can setup reader that builds structure from local fs (via file/dir reads)
--  ..tracking what is substantiated locally vs. not is hard.. must be in repostate! can do.
--  note: done (probably works)

-- main :: IO ()
-- main = do
--   putStrLn "ok so start doin the main fn"
--   store <- mkStore
--   putStrLn "a"
--   pb  <- readAndStore store "examples/before"
--   putStrLn "b"
--   pa1 <- readAndStore store "examples/after1"
--   putStrLn "b"
--   pa2 <- readAndStore store "examples/after2"
--   pa3 <- readAndStore store "examples/after3"

--   ba1Diffs <- diffMerkleDirs store pb pa1
--   putStrLn "before -> after1"
--   print ba1Diffs

--   ba2Diffs <- diffMerkleDirs store pb pa2
--   putStrLn "before -> after2"
--   print ba2Diffs

--   ba3Diffs <- diffMerkleDirs store pb pa3
--   putStrLn "before -> after3"
--   print ba3Diffs

main :: IO ()
main = parse >>= \case
  InitRepo -> do
    mkHgitDir -- mk .hgit and .hgit/store dirs
    writeState initialRepoState
  CheckoutBranch branch pathMatchers -> do
    store <- mkStore
    repostate <- readState
    -- TODO: just wipe everything (?)
    clearRepoAction <- undefined repostate
    clearRepoAction
    targetCommit <- getBranch branch repostate -- throw if branch name not known
    undefined pathMatchers $ lazyDeref store targetCommit -- deref and write filtering based on pathMatchers
  MkBranch branch -> do
    repostate <- readState
    current   <- getBranch (currentBranch repostate) repostate
    writeState $ repostate
               { branches      = branches repostate ++ [(branch, getConst current)]
               , currentBranch = branch
               }-- same pointer as current, same dir state - just add new branch -> pointer mapping
  MkCommit msg -> do
    store             <- mkStore
    repostate         <- readState
    base              <- baseDir
    currentCommitHash <- getBranch (currentBranch repostate) repostate
    currentStateHash  <- readAndStore store base
    let commit = Commit msg currentStateHash currentCommitHash
    hash <- sUploadShallow store commit
    writeState $ repostate
               { branches = (\(x,p) ->
                               if x == (currentBranch repostate)
                                 then (x, p)
                                 else (x, getConst hash)
                            )
                        <$> branches repostate
               }

  GetStatus -> do
    store             <- mkStore
    repostate         <- readState
    base              <- baseDir
    currentCommitHash <- getBranch (currentBranch repostate) repostate
    currentStateHash  <- readAndStore store base

    currentCommit   <- sDeref store currentCommitHash

    diffs     <- diffMerkleDirs store (commitRoot currentCommit) currentStateHash
    print diffs

  GetDiff branch1 branch2 -> do
    store     <- mkStore
    repostate <- readState
    commitp1  <- getBranch branch1 repostate
    commitp2  <- getBranch branch2 repostate
    commit1   <- sDeref store commitp1
    commit2   <- sDeref store commitp2
    diffs     <- diffMerkleDirs store (commitRoot commit1) (commitRoot commit2)
    print diffs



-- IDEA: use 'Pair (Const HashPointer) f' instead of (,) HashPointer :+ f
commitRoot
  :: HGit (Term (FC.Compose HashIndirect :++ HGit)) 'CommitTag
  -> Const HashPointer 'DirTag
commitRoot (Commit _ (Term (HC (FC.Compose (C (p, _))))) _) = Const p
commitRoot NullCommit        = emptyDirHash
