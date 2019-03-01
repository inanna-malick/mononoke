module Main where

--------------------------------------------
import           Control.Monad.Except (runExceptT)
import           Control.Monad.IO.Class (MonadIO, liftIO)
--------------------------------------------
import           Compare (compareMerkleTrees)
import           FileIO (writeTree, readTree)
import           Merkle.Tree.Types
import           Util.MyCompose
import           Util.Util (mapErrUtil)
import           Util.RecursionSchemes
import           Merkle.Store
import           Merkle.Store.Deref
import           Merkle.Store.FileSystem
--------------------------------------------

-- TODO: new app plan - minimum required for cool demo, basically - idea is diffing branches, checking them out, etc
-- init: zero args, creates branch 'master'
-- add-all: commit message, adds everything in current directory via new commit
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
-- need branch command to create new branch
-- might as well have status command - diff current w/e and etc


-- NOTE: hgit is a nice pun name (unintentional), includes features of both hg and git comma lmao

-- NOTE: might as well just keep popping up the directory tree to find that hgit file w/ branch mappings anyway, lol - (or .hgit/branches, with .hgit/store as the store (removes need for flag))

-- requires: xyz
-- new work: .git file and store (oh hey, I can make the store directory part of the 'repo' and do everything at that level) -let's say it's also aeson of this data structure:
 -- todo import and use map? or just object w/ mappings via alias?
-- eg: data RepoData = RepoData [(String, HashPointer)] (string to int map, tolerable usage of json)
-- this is essentially the hg approach

-- note: can just throw on error for conciseness in store pointer failure case
--       (but still make everything polymorphic wrt m for later if I decide otherwise)
-- DONE: using MonadThrow, etc. Nice.

main :: IO ()
main = undefined
