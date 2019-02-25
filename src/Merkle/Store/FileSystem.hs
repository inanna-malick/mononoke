module Merkle.Store.FileSystem where

--------------------------------------------
import qualified Data.Aeson as AE
import qualified Data.Aeson.Encoding as AE (encodingToLazyByteString)
import qualified Data.Aeson.Parser.Internal as AE

import           Control.Monad.Except
import qualified Data.ByteString.Lazy as B
import qualified Data.Hashable as Hash
import           System.Directory (getTemporaryDirectory, createDirectory)
import           System.Random (randomIO)
--------------------------------------------
import           Errors
import           Merkle.Tree.Types
import           Merkle.Tree.Encoding
import           Util.HRecursionSchemes
import           Util.MyCompose
import           Merkle.Store
--------------------------------------------
import Data.Functor.Const
import qualified Data.Functor.Compose as FC
import           Data.Singletons


-- | Filesystem backed store using a temp dir
tmpFsStore :: IO $ Store  (ExceptT MerkleTreeLookupError IO) HGit
tmpFsStore = do
  dir <- createTmpDir "merklestore"
  pure $ fsStore dir

-- | Filesystem backed store using the provided dir
fsStore :: FilePath -> Store (ExceptT MerkleTreeLookupError IO) HGit
fsStore root
  = Store
  { sDeref = \p -> do
      liftIO . putStrLn $ "attempt to deref " ++ show p ++ " via fs state store"
      contents <- liftIO $ B.readFile (root ++ "/" ++ f p)
      case (AE.decode contents) of
        Nothing -> throwError $ EntityNotFoundInStore p
        Just x  -> do
          pure $ hfmap (\(Const p) -> Term $ HC $ FC.Compose $ C (p, Nothing)) x
  , sUploadShallow = \smtl -> do
      let e = AE.encodingToLazyByteString $ AE.toEncoding $ encode smtl
          p = Hash.hash e
      liftIO $ B.writeFile (root ++ "/" ++ f p) e
      pure p
  }
  where

    -- todo: make this bidirectional so I can use it as an input format
    chars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
    base = length chars

    f n | n == 0 = ""
        | n < 0 = f $ (-1) * n -- increase risk of hash colissions here, but #YOLO
        | otherwise = chars !! (n `rem` base) : f (n `div` base)

-- todo this should really be bracket pattern for cleanup
createTmpDir :: String -> IO FilePath
createTmpDir prefix = do
  sysTmp <- getTemporaryDirectory
  x <- randomIO -- collision detection? lmao no,lol  #YOLO
  let dir = sysTmp ++ "/" ++ prefix ++ show (x :: Int)
  createDirectory dir
  putStrLn $ "created temp dir: " ++ dir
  pure dir
