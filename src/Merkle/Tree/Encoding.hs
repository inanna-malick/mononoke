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

-- | named merkle tree in which the top layer is known to be substantiated and
--   all sub-nodes are represented using hash addressed pointers
--   NOTE: leaf nodes are themselves merkle trees that can either be substantiated leaf nodes (inlined)
--         or merkle trees of arbitrary size in which nodes use pointer-indirection
--   newtype and not type alias so we can have clean typeclass instances
newtype ShallowNamedMerkleTreeLayer
  = SNMTL
  -- named tree layer in which all leaf file trees are pointer tagged and substantiated? shit nah lol du'nt work
  { unSNMTL
    :: (Named :+ Tree ShallowMerkleTreeLayer) Pointer
  }

instance Hashable ShallowNamedMerkleTreeLayer where
  hashWithSalt s (SNMTL (C (n, (Leaf contents))))
    = s `hashWithSalt` hash n `hashWithSalt` hash contents
  hashWithSalt s (SNMTL (C (n, (Node contents))))
    = s `hashWithSalt` hash n `hashWithSalt` hash (fmap unPointer contents)

instance ToJSON ShallowNamedMerkleTreeLayer where
    -- this generates a Value
    toJSON (SNMTL (C (name, (Leaf contents)))) =
        object [ "type" .= ("leaf" :: Text)
               , "name" .= pack name
               , "contents" .= toJSON contents
               ]
    toJSON (SNMTL (C (name, (Node pointers)))) =
        object [ "type" .= ("node" :: Text)
               , "name" .= pack name
               , "children" .= toJSON (fmap unPointer pointers)
               ]

instance FromJSON ShallowNamedMerkleTreeLayer where
    parseJSON = withObject "ShallowNamedMerkleTreeLayer" $ \v -> do
        name <- v .: "name"
        typ  <- v .: "type"
        case typ of
          "node" -> do
              children <- fmap Pointer <$> v .: "children"
              pure . SNMTL $ C (name, Node children)
          "leaf" -> do
              contents <- v .: "contents"
              pure . SNMTL $ C (name, Leaf contents)
          x -> fail $ "unsupported node type " ++ x


-- | merkle tree in which the top layer is known to be substantiated and
--   all sub-nodes are represented using hash addressed pointers
-- NOTE: so leafs are concrete strings and nodes are pointer indirection
--   newtype and not type alias so we can have clean typeclass instances
newtype ShallowMerkleTreeLayer = SMTL { unSMTL :: Tree FileChunk Pointer}
  deriving (Eq, Show)

instance Hashable ShallowMerkleTreeLayer where
  hashWithSalt s (SMTL (Leaf contents))
    = s `hashWithSalt` hash contents
  hashWithSalt s (SMTL (Node contents))
    = s `hashWithSalt` hash (fmap unPointer contents)

instance ToJSON ShallowMerkleTreeLayer where
    -- this generates a Value
    toJSON (SMTL (Leaf body)) =
        object [ "type" .= ("leaf" :: Text)
               , "body" .= pack body
               ]
    toJSON (SMTL (Node pointers)) =
        object [ "type" .= ("node" :: Text)
               , "children" .= toJSON (fmap unPointer pointers)
               ]

instance FromJSON ShallowMerkleTreeLayer where
    parseJSON = withObject "ShallowMerkleTreeLayer" $ \v -> do
        typ  <- v .: "type"
        case typ of
          "node" -> do
              children <- fmap Pointer <$> v .: "children"
              pure . SMTL $ Node children
          "leaf" -> do
              body <- v .: "body"
              pure . SMTL $ Leaf body
          x -> fail $ "unsupported node type " ++ x
