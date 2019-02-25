{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Merkle.Tree.Encoding where

--------------------------------------------
import           Data.Aeson
import           Data.Hashable (Hashable(..))
import           Data.Text
--------------------------------------------
import           Merkle.Types (Pointer(..))
import           Merkle.Tree.Types
import           Util.MyCompose
--------------------------------------------

import qualified Util.HRecursionSchemes as C -- YOLO 420 SHINY AND CHROME
import           Util.HRecursionSchemes ((:->)) -- YOLO 420 SHINY AND CHROME
import Data.Functor.Const
import Data.Functor.Compose (Compose(..))
-- import qualified Data.Comp.Multi.HFunctor as C
-- import qualified Data.Comp.Multi.Term as C

-- | merkle tree in which the top layer is known to be substantiated and
--   all sub-nodes are represented using hash addressed pointers
--   newtype and not type alias so we can have clean typeclass instances
-- newtype ShallowMerkleTreeLayer = SMTL { unSMTL :: (Named :+ Tree) Pointer



-- todo - use existential quant trick to pare this down to just parsing a blob or aeson value
-- IDEA: have as Aeson.Value -> HGit xyz i - anyway, isomorphic to current, can do later
-- class FetchHashPointer (x :: HGitTags) where
--   fetch :: HashPointer -> HGit (Const HashPointer) x

-- instance FetchHashPointer DirTag       where fetch _p = undefined
-- instance FetchHashPointer FileChunkTag where fetch _p = undefined
-- instance FetchHashPointer MetaTag      where fetch _p = undefined
-- instance FetchHashPointer CommitTag    where fetch _p = undefined

magic2 :: SingHGT x -> HashPointer -> HGit (Const HashPointer) x
magic2 s _p = case s of
  SFileChunkTag -> (undefined :: HGit (Const HashPointer) FileChunkTag)
  SDirTag       -> (undefined :: HGit (Const HashPointer) DirTag)
  SCommitTag    -> (undefined :: HGit (Const HashPointer) CommitTag)
  SMetaTag      -> (undefined :: HGit (Const HashPointer) MetaTag)

fakeStore :: HFStore IO
fakeStore s p = pure . C.hfmap f $ magic2 s p
  where f (Const p) = C.Term $ HC $ Compose $ C (p, Nothing)

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

-- instance FromJSON ShallowMerkleTreeLayer where
--     parseJSON = withObject "ShallowMerkleTreeLayer" $ \v -> do
--         name <- v .: "name"
--         typ  <- v .: "type"
--         case typ of
--           "node" -> do
--               children <- fmap Pointer <$> v .: "children"
--               pure . SMTL $ C (name, Node children)
--           "leaf" -> do
--               body <- v .: "body"
--               pure . SMTL $ C (name, Leaf body)
--           x -> fail $ "unsupported node type " ++ x
