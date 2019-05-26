module Main where

--------------------------------------------
import           Control.Monad.Reader (runReaderT)
--------------------------------------------
import           Merkle.Store (liftShallowStore)
import           Merkle.Store.IPFS
import qualified Merkle.Store.MockIPFS as MockIPFS
import           Runtime.Capabilities
import           Runtime.Commands
import           Runtime.RunCmd
--------------------------------------------


main :: IO ()
main = parse >>= \case
  (ipfsNode, Left InitRepo) -> initRepo ipfsNode
  (ipfsNode, Right repoCmd) -> do
    base  <- hgitBaseDir
    state <- readState

    let stores = case ipfsNode of
          Left node ->
            HgitStore (liftShallowStore $ ipfsStore node)
                      (liftShallowStore $ ipfsStore node)
                      (liftShallowStore $ ipfsStore node)
          Right mockNodeFp ->
            HgitStore (liftShallowStore $ MockIPFS.mockIpfsStore mockNodeFp)
                      (liftShallowStore $ MockIPFS.mockIpfsStore mockNodeFp)
                      (liftShallowStore $ MockIPFS.mockIpfsStore mockNodeFp)

        caps = RepoCaps stores state base
    mNextState <- runReaderT (runCommand repoCmd) caps
    maybe (pure ()) writeState mNextState
