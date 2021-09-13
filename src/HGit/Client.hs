{-# language CPP                        #-}
{-# language DataKinds                  #-}
{-# language DeriveGeneric              #-}
{-# language DerivingVia                #-}
{-# language FlexibleContexts           #-}
{-# language FlexibleInstances          #-}
{-# language GADTs                      #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses      #-}
{-# language OverloadedStrings          #-}
{-# language PolyKinds                  #-}
{-# language QuasiQuotes                #-}
{-# language ScopedTypeVariables        #-}
{-# language StandaloneDeriving         #-}
{-# language TemplateHaskell            #-}
{-# language TypeFamilies               #-}
{-# language TypeOperators              #-}
{-# language UndecidableInstances       #-}
{-# language OverloadedLabels #-}
{-# language DuplicateRecordFields #-}
{-# language DeriveAnyClass #-}

module HGit.Client where

import Data.Aeson as AE
import           GHC.Generics
import           Mu.Quasi.GRpc
import           Mu.Schema hiding (Term(..))
import           Mu.GRpc.Client.TyApps
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Functor.Const (Const(..))
import HGit.Generic.HRecursionSchemes
import HGit.Core.Types (M, MTag)
import           Data.Singletons.TH
import Data.Word
import Control.Monad.State as S
import qualified HGit.Generic.BlakeHash as BH;
import Data.Functor.Identity
import Data.Functor.Classes
import Data.ByteString.Builder as BB (word32LE, toLazyByteString)

grpc "GRPCStore" id "dagstore.proto"

data Id
  = Id { id_data :: Word32 }
  deriving (Eq, Ord, Show, Generic)

-- TODO: ToJSON, split this file the fuck up lol
instance ToJSON Id where
    toEncoding = toEncoding . id_data
instance FromJSON Id where
    parseJSON = fmap Id . parseJSON

instance ToSchema   GRPCStore "Id" Id
instance FromSchema GRPCStore "Id" Id


data Hash
  = Hash { hash_data :: B.ByteString }
  deriving (Eq, Ord, Show, Generic)

instance ToSchema   GRPCStore "Hash" Hash
instance FromSchema GRPCStore "Hash" Hash

data Header f
  = Header
  { header_id :: f Id
  , header_hash :: f Hash
  } deriving (Generic)

instance Ord1 f => Ord (Header f) where
  compare (Header id1 h1) (Header id2 h2) = compare1 id1 id2 <> compare1 h1 h2

instance Eq1 f => Eq (Header f) where
  (Header id1 h1) == (Header id2 h2) = eq1 id1 id2 && eq1 h1 h2

instance Show1 f => Show (Header f) where
  showsPrec n (Header id1 h1) = showString "id: " . showsPrec1 n id1 . showString ", hash: " . showsPrec1 n h1

instance ToSchema   GRPCStore "Header" (Header Maybe)
instance FromSchema GRPCStore "Header" (Header Maybe)

data Node f
  = Node
  { node_data :: B.ByteString
  , node_links :: [Header f]
  } deriving (Eq, Ord, Show, Generic)

instance ToSchema   GRPCStore "Node" (Node Maybe)
instance FromSchema GRPCStore "Node" (Node Maybe)

data NodeWithHeaderP
  = NodeWithHeaderP
  { header :: Maybe (Header Maybe)
  , node :: Maybe (Node Maybe)
  } deriving (Eq, Ord, Show, Generic)

instance ToSchema   GRPCStore "NodeWithHeader" NodeWithHeaderP
instance FromSchema GRPCStore "NodeWithHeader" NodeWithHeaderP

data GetRespP
  = GetRespP
  { requested_node :: Maybe (Node Maybe)
  , extra_nodes :: [NodeWithHeaderP]
  } deriving (Eq, Ord, Show, Generic)

instance ToSchema   GRPCStore "GetResp" GetRespP
instance FromSchema GRPCStore "GetResp" GetRespP


defaultConfig :: GrpcClientConfig
defaultConfig = grpcClientConfigSimple "127.0.0.1" 8008 False

mkClientOrThrow :: IO GrpcClient
mkClientOrThrow = do
    Right (client) <- setup defaultConfig
    pure client
  where setup config = setupGrpcClient' config


get :: GrpcClient -> Hash -> IO (Either String GetRespP)
get client hash = do
  putStrLn $ "GET node"
  response :: GRpcReply GetRespP
    <- gRpcCall @'MsgProtoBuf @DagStore @"DagStore" @"GetNode" client hash
  case response of
    GRpcOk h -> pure $ Right h
    x -> do
      putStrLn $ "GET: error response was: " ++ show response
      pure $ Left $ show x


put :: GrpcClient -> Node Maybe -> IO (Either String Hash)
put client to_put = do
  putStrLn $ "PUT node"
  response :: GRpcReply Hash
    <- gRpcCall @'MsgProtoBuf @DagStore @"DagStore" @"PutNode" client to_put
  case response of
    GRpcOk h -> pure $ Right h
    x -> do
      putStrLn $ "PUT: error response was: " ++ show response
      pure $ Left $ show x



idToHash
  :: forall (i :: MTag)
   . SingI i
  => [(Id, BH.RawBlakeHash)]
  -> M (Const Id) i
  -> M BH.Hash i
idToHash headers m = hfmap f m
  where
    f :: Const Id :-> BH.Hash
    f (Const idp) = Const (maybe (error "TODO") id $ lookup idp headers)


hashToId
  :: forall (i :: MTag)
   . SingI i
  => M BH.Hash i
  -> (M (Const Id) i, [(BH.RawBlakeHash, Id)])
hashToId m = flip runState [] $ hmapM f m
  where
    f :: NatM (State [(BH.RawBlakeHash, Id)])
               BH.Hash
              (Const Id)
    f (Const rawHash) = do
      mappings <- S.get
      let nextId    = Id $ fromInteger $ toInteger $ length mappings
          mapping   = (rawHash, nextId)
          mappings' = mappings ++ [mapping]
      S.put mappings'
      pure $ Const nextId

toProtoM :: M BH.Hash :=> Node Maybe
toProtoM m = Node
           { node_data  = BL.toStrict $ encode m'
           , node_links = headers
           }
  where
    (m', mappings) = hashToId m
    headers = fmap f mappings
    f (rh, idp) = Header
                { header_id = Just $ idp
                , header_hash = Just $ Hash $ BH.unpackHash' rh
                }



-- Blake2s
toCanonicalHash :: Node Identity -> BH.RawBlakeHash
toCanonicalHash n = BH.doHash' $ links ++ [node_data n]
  where
    links = foldMap f (node_links n)
    f h = [ BL.toStrict $ BB.toLazyByteString $ word32LE $ id_data $ runIdentity $ header_id h
          , hash_data $ runIdentity $ header_hash h
          ]


-- use blake2::Digest;
-- let mut hasher = blake2::Blake2s::new();
-- for link in self.links.iter() {
--     hasher.update(&link.id.0.to_be_bytes());
--     hasher.update(link.hash.0.as_slice());
-- }
-- hasher.update(&self.data.0);
-- let hash = hasher.finalize();
-- Hash(hash)



-- ughhhh the haskell one is C so in IO fuck this noise
-- can't use own local hashing alg b/c requires full local tree to hash
-- just downgrade rust end to blake2 lmao fuk




-- -- parse a single node
-- fromProtoM
--   :: forall (i :: MTag)
--    . SingI i
--   => Const NodeP i
--   -> Either String (M Hash i)
-- fromProtoM (Const NodeP{..}) =
--   let f HeaderP{..} = do
--         header_id'  <- maybe (Left "Header missing Id") id header_id
--         header_hash' <- maybe (Left "Header missing Hash") id header_hash
--         (header_id', hash)
        
--   mappings  <- traverse f node_links
--   m <- case fromJSON node_data of
--          Error e   -> Left $ "Error decoding node body: " ++ show e
--          Success x -> pure x
--   pure $ idToHash mappings m
  


type PartialTree = Term (M `HCompose` HEither BH.Hash)

-- recursively parse nodes building up a tree of them, TODO: a type for that, term of HEither Hash M
fromProtoGetResp :: forall (i :: MTag). SingI i => Const GetRespP i -> Either String (PartialTree i)
fromProtoGetResp = undefined
