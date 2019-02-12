module Merkle.Store.FileSystem where

--------------------------------------------
import qualified Data.Aeson as AE
import           Control.Monad.Except
import qualified Data.ByteString.Lazy as B
import qualified Data.Hashable as Hash
import           System.Directory (getTemporaryDirectory, createDirectory)
import           System.Random (randomIO)
--------------------------------------------
import           Errors
import           Merkle.Types (Pointer(..), makeConcrete)
import           Util.MyCompose
import           Merkle.Store
--------------------------------------------

-- | Filesystem backed store using a temp dir
tmpFsStore
  :: forall f a
   . ( AE.FromJSON a
     , AE.ToJSON a
     , Hash.Hashable a
     , Functor f
     )
  => (f Pointer -> a)
  -> (a -> f Pointer)
  -> IO $ Store  (ExceptT MerkleTreeLookupError IO) f
tmpFsStore wrap unwrap = do
  dir <- createTmpDir "merklestore"
  pure $ fsStore wrap unwrap dir

-- | Filesystem backed store using the provided dir
fsStore
  :: forall f a
   . ( AE.FromJSON a
     , AE.ToJSON a
     , Hash.Hashable a
     , Functor f
     )
  => (f Pointer -> a)
  -> (a -> f Pointer)
  -> FilePath
  -> Store (ExceptT MerkleTreeLookupError IO) f
fsStore wrap unwrap root
  = Store
  { sDeref = \p -> do
      liftIO . putStrLn $ "attempt to deref " ++ show p ++ " via fs state store"
      contents <- liftIO $ B.readFile (root ++ "/" ++ f p)
      -- liftIO . putStrLn $ "returning deref res via fs state store: " ++ show contents
      case AE.decode contents of
        Nothing -> throwError $ EntityNotFoundInStore p
        Just x  -> do
          pure $ makeConcrete $ unwrap x
  , sUploadShallow = \smtl -> do
      let p = Pointer $ Hash.hash $ wrap smtl
      liftIO $ B.writeFile (root ++ "/" ++ f p) (AE.encode $ wrap smtl)
      pure p
  }
  where
    f (Pointer p) = h p

    -- todo: make this bidirectional so I can use it as an input format
    chars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
    base = length chars

    h n | n == 0 = ""
        | n < 0 = h $ (-1) * n -- increase risk of hash colissions here, but #YOLO
        | otherwise = chars !! (n `rem` base) : h (n `div` base)

-- todo this should really be bracket pattern for cleanup
createTmpDir :: String -> IO FilePath
createTmpDir prefix = do
  sysTmp <- getTemporaryDirectory
  x <- randomIO -- collision detection? lmao no,lol  #YOLO
  let dir = sysTmp ++ "/" ++ prefix ++ show (x :: Int)
  createDirectory dir
  putStrLn $ "created temp dir: " ++ dir
  pure dir
