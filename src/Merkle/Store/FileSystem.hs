{-# LANGUAGE QuantifiedConstraints #-}

module Merkle.Store.FileSystem where

--------------------------------------------
import           Control.Exception.Safe (MonadThrow, throw)
import           Control.Monad.Except
import qualified Data.Aeson as AE
import qualified Data.Aeson.Encoding as AE (encodingToLazyByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Functor.Compose
import           Data.Functor.Const
--------------------------------------------
import           Errors
import           Merkle.Store
import           Merkle.Types
import           Util.RecursionSchemes
--------------------------------------------

-- | Filesystem backed store using the provided dir
fsStore
  :: forall m f
   . ( MonadIO m
     , MonadThrow m
     , Functor f
     , Hashable f
     , AE.ToJSON1   f
     , AE.FromJSON1 f
     )
  => FilePath
  -> Store m f
fsStore root
  = Store
  { sDeref = \p -> do
      -- liftIO . putStrLn $ "attempt to deref " ++ show p ++ " via fs state store @ " ++ fn
      contents <- liftIO $ B.readFile (root ++ "/" ++ fn p)
      case AE.eitherDecodeStrict contents of
        Left  e -> throw . DecodeError $ show e
        -- todo: actually figure out a strategy re: this
        Right (HashTerm x) -> pure $ fmap (\p' -> Fix $ Compose (p', Compose Nothing)) x

  , sUploadShallow = \x -> do
      let p = hash x
      liftIO . BL.writeFile (root ++ "/" ++ fn p)
             . AE.encodingToLazyByteString
             . AE.toEncoding
             $ HashTerm x

      pure p
  }
  where
    fn :: Hash i -> String
    fn = show . hashToText . getConst
