
module Runtime.Commands (MetaCommand(..), RepoCommand(..), parse) where

import Options.Applicative
import Data.Semigroup ((<>))

import HGit.Core.Types
import Merkle.Store.IPFS


parse :: IO (IPFSNode, MetaCommand `Either` RepoCommand)
parse = execParser opts
  where
    opts = info (parser <**> helper)
      ( fullDesc
     <> progDesc "do some git/mercurial type stuff, backed by local IPFS daemon"
     <> header "hgit - an implementation of core git/mercurial features using recursion schemes" )

-- initialize repo structure
data MetaCommand
  = InitRepo
  -- | InitServer Int

data RepoCommand
  -- switch directory state to that of new branch (nuke and rebuild via store)
  -- fails if any changes exist in current dir (diff via status /= [])
  = CheckoutBranch BranchName
  -- neither look at or modify filesystem state
  -- create new branch with same root commit as current branch. changes are fine
  | MkBranch BranchName
  -- merge some branch into the current one (requires no changes)
  | MkMergeCommit BranchName CommitMessage
  | MkCommit CommitMessage
  | GetStatus -- get status of current repo (diff current state vs. that of last commit on branch)
  | GetDiff BranchName BranchName

parser :: Parser (IPFSNode, MetaCommand `Either` RepoCommand)
parser
  = (,) <$> ipfsNodeParser <*> subparser
      ( command "checkout" (info (fmap Right checkoutOptions) ( progDesc "checkout a branch"     ))
      <> command "branch"   (info (fmap Right branchOptions)   ( progDesc "create a new branch"   ))
      <> command "init"     (info (fmap Left  initROptions)    ( progDesc "create a new repo"     ))
      <> command "commit"   (info (fmap Right commitOptions)   ( progDesc "create a new commit"   ))
      <> command "status"   (info (fmap Right statusOptions)   ( progDesc "show repo status"      ))
      <> command "diff"     (info (fmap Right diffOptions)     ( progDesc "show diff of branches" ))
      <> command "merge"    (info (fmap Right mergeOptions)    ( progDesc "merge a branch into the current one" ))
      )
  where
    checkoutOptions
        = CheckoutBranch
      <$> strArgument
          ( metavar "BRANCHNAME"
         <> help "branch to checkout"
          )
    branchOptions
        = MkBranch
      <$> strArgument
          ( metavar "BRANCHNAME"
         <> help "branch to create"
          )
    -- initSOptions  = pure $ InitServer 8888 -- todo: impl parser here!
    initROptions  = pure InitRepo
    commitOptions
        = MkCommit
      <$> strArgument
          ( metavar "MESSAGE"
         <> help "commit msg"
          )
    mergeOptions
        = MkMergeCommit
      <$> strArgument
          ( metavar "BRANCHNAME"
         <> help "branch to merge into the current one"
          )
      <*> strArgument
          ( metavar "MESSAGE"
         <> help "commit msg"
          )
    statusOptions  = pure GetStatus
    diffOptions
        = GetDiff
      <$> strArgument
          ( metavar "BEFORE"
         <> help "'before' branch name"
          )
      <*> strArgument
          ( metavar "AFTER"
         <> help "'after' branch name"
          )

ipfsNodeParser :: Parser IPFSNode
ipfsNodeParser = IPFSNode
             <$> strOption
                ( long "host"
               <> short 'h'
               <> metavar "IPFS-DAEMON-HOST"
               <> showDefault
               <> value (host localHost)
               <> help "ipfs daemon host"
                )
            <*> option auto
                ( long "port"
               <> short 'p'
               <> metavar "IPFS-DAEMON-HOST"
               <> showDefault
               <> value (port localHost)
               <> help "ipfs daemon port"
                )
