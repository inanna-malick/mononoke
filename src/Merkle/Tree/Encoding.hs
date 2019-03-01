{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Merkle.Tree.Encoding where

--------------------------------------------
import           Data.Aeson
import           Data.Aeson.Types (Parser(..))
import           Data.Hashable (Hashable(..))
import           Data.Text
--------------------------------------------
import           Merkle.Tree.Types
import           Util.MyCompose
--------------------------------------------

import           Data.Singletons
import qualified Util.HRecursionSchemes as C -- YOLO 420 SHINY AND CHROME
import           Util.HRecursionSchemes ((:->)) -- YOLO 420 SHINY AND CHROME
import Data.Functor.Const
import Data.Functor.Compose (Compose(..))


instance SingI x => FromJSON (HGit (Const HashPointer) x) where
  parseJSON = sdecode sing

sdecode :: Sing x -> Value -> Parser $ HGit (Const HashPointer) x
sdecode = \case
  SFileChunkTag -> withObject "HGit (Const HashPointer) FileChunkTag" $ \v -> do
        typ  <- v .: "type"
        case typ of
          "blobtree" -> do
              children <- v .: "children"
              pure $ BlobTree $ fmap Const children
          "blob" -> do
              contents <- v .: "contents"
              pure $ Blob contents
          x -> fail $ "require [blob, blobtree] type" ++ x

  SDirTag       -> withObject "HGit (Const HashPointer) DirTag" $ \v -> do
        name <- v .: "name"
        typ  <- v .: "type"
        case typ of
          "dir" -> do
              children <- v .: "children"
              pure $ Dir name $ fmap Const children
          "file" -> do
              body <- v .: "body"
              pure $ File name $ Const body
          x -> fail $ "require [file, dir] type" ++ x

  SCommitTag    -> withObject "HGit (Const HashPointer) CommitTag" $ \v -> do
        typ  <- v .: "type"
        case typ of
          "nullcommit" -> pure NullCommit
          "commit" -> do
              name <- v .: "name"
              root <- v .: "root"
              prev <- v .: "prev"
              pure $ Commit name (Const root) (Const prev)
          x -> fail $ "require [commit, nullcommit] type" ++ x


encode :: HGit (Const HashPointer) x -> Value
encode = \case
    Blob contents ->
        object [ "type" .= ("blob" :: Text)
               , "contents" .= pack contents
               ]
    BlobTree children ->
        object [ "type" .= ("blobtree" :: Text)
               , "children" .= fmap getConst children
               ]

    File name contents ->
        object [ "type" .= ("file" :: Text)
               , "name" .= pack name
               , "contents" .= getConst contents
               ]
    Dir name children ->
        object [ "type" .= ("dir" :: Text)
               , "name" .= pack name
               , "children" .= fmap getConst children
               ]

    Commit name root prev ->
        object [ "type" .= ("commit" :: Text)
               , "name" .= pack name
               , "root" .= getConst root
               , "prev" .= getConst prev
               ]
    NullCommit ->
        object [ "type" .= ("nullcommit" :: Text)
               ]

-- instance Hashable ShallowMerkleTreeLayer where
--   hashWithSalt s (SMTL (C (n, (Leaf contents))))
--     = s `hashWithSalt` hash n `hashWithSalt` hash contents
--   hashWithSalt s (SMTL (C (n, (Node contents))))
--     = s `hashWithSalt` hash n `hashWithSalt` hash (fmap unPointer contents)

-- instance ToJSON ShallowMerkleTreeLayer where
--     -- this generates a Value
--     toJSON (SMTL (C (name, (Leaf body)))) =
--         object [ "type" .= ("leaf" :: Text)
--                , "name" .= pack name
--                , "body" .= pack body
--                ]
--     toJSON (SMTL (C (name, (Node pointers)))) =
--         object [ "type" .= ("node" :: Text)
--                , "name" .= pack name
--                , "children" .= toJSON (fmap unPointer pointers)
--                ]
