module HGit.Store.FileSystem where

--------------------------------------------
import qualified Data.Aeson as AE
import qualified Data.Aeson.Encoding as AE (encodingToLazyByteString)

import           Control.Exception.Safe (MonadThrow, throw)
import           Control.Monad.Except
import qualified Data.ByteString.Lazy as B
import qualified Data.Functor.Compose as FC
import           Data.Functor.Const (Const(..))
import qualified Data.Hashable as Hash
--------------------------------------------
import           Errors
import           HGit.Serialization
import           HGit.Store
import           HGit.Types
import           Util.HRecursionSchemes
import           Util.MyCompose
--------------------------------------------

-- | Filesystem backed store using the provided dir
fsStore
  :: MonadIO m
  => MonadThrow m
  => FilePath
  -> Store m HGit
fsStore root
  = Store
  { sDeref = \p -> do
      liftIO . putStrLn $ "attempt to deref " ++ show p ++ " via fs state store"
      contents <- liftIO $ B.readFile (root ++ "/" ++ f p)
      case (AE.decode contents) of
        Nothing -> throw $ EntityNotFoundInStore p
        Just x  -> do
          pure $ hfmap (\(Const p') -> Term $ HC $ FC.Compose $ C (p', Nothing)) x
  , sUploadShallow = \smtl -> do
      let e = AE.encodingToLazyByteString $ AE.toEncoding $ sencode smtl
          p = Hash.hash e
      liftIO $ B.writeFile (root ++ "/" ++ f p) e
      pure p
  }
  where
    chars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
    base = length chars

    f n | n == 0 = ""
        | n < 0 = f $ (-1) * n -- increase risk of hash collisions here by 2x, but #YOLO
        | otherwise = chars !! (n `rem` base) : f (n `div` base)
