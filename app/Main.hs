module Main where

--------------------------------------------
import           Control.Monad.Reader (runReaderT)
--------------------------------------------
import           Merkle.Store (liftShallowStore)
import           Merkle.Store.IPFS
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
    let stores = HgitStore (liftShallowStore $ ipfsStore ipfsNode)
                           (liftShallowStore $ ipfsStore ipfsNode)
                           (liftShallowStore $ ipfsStore ipfsNode)
        caps = RepoCaps stores state base
    mNextState <- runReaderT (runCommand repoCmd) caps
    maybe (pure ()) writeState mNextState
