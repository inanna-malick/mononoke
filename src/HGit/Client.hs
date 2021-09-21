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
{-# language ScopedTypeVariables        #-}
{-# language StandaloneDeriving         #-}
{-# language TemplateHaskell            #-}
{-# language TypeFamilies               #-}
{-# language TypeOperators              #-}
{-# language UndecidableInstances       #-}
{-# language OverloadedLabels           #-}
{-# language DuplicateRecordFields      #-}
{-# language DeriveAnyClass             #-}
{-# language NamedFieldPuns             #-}
{-# language RankNTypes                 #-}

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
import           HGit.Core.Types (decodeM, M, MTag)
import           Data.Singletons.TH
import Data.Word
import qualified Control.Monad.State as S
import qualified HGit.Generic.BlakeHash as BH;
import Data.Functor.Identity
import Data.Functor.Classes
import Data.ByteString.Builder as BB (word32LE, toLazyByteString)
import Control.Monad.Except

grpc "GRPCStore" id "dagstore.proto"

data Id
  = Id { id_data :: Word32 }
  deriving (Eq, Ord, Show, Generic)

-- TODO: ToJSON, split this file the fuck up lol
instance ToJSON Id where
    toJSON = toJSON . id_data
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

requireFields :: Node Maybe -> Maybe (Node Identity)
requireFields (Node nd nl) =  Node nd <$> traverse requireHeaderFields nl


requireHeaderFields :: Header Maybe -> Maybe (Header Identity)
requireHeaderFields (Header (Just hid) (Just hh)) = Just $ Header (Identity hid) (Identity hh)
requireHeaderFields (Header _ _)                  = Nothing

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
defaultConfig = grpcClientConfigSimple "127.0.0.1" 8080 False

mkClient :: ExceptT String IO GrpcClient
mkClient = do
    eclient <- setup defaultConfig
    case eclient of
      Left e -> throwError $ show e
      Right client -> pure client
  where setup config = setupGrpcClient' config


get
  :: GrpcClient
  -> NatM (ExceptT String IO) BH.Hash (M BH.Hash)
get client (Const h) = do
  let hash = Hash $ BH.unpackHash' h
  lift $ putStrLn $ "GET node"
  response :: GRpcReply GetRespP
    <- lift $ gRpcCall @'MsgProtoBuf @DagStore @"DagStore" @"GetNode" client hash
  case response of
    GRpcOk (GetRespP Nothing _) -> throwError "no node on get resp"
    GRpcOk (GetRespP (Just pn) _) -> liftEither $ do
      n <- maybe (Left "missing fields on proto resp node") Right $ requireFields pn
      m <- fromProto $ Const n
      Right m
    x -> throwError $ "GET: error response was: " ++ show x


get'
  :: GrpcClient
  -> BH.RawBlakeHash
  -> ExceptT String IO GetRespP
get' client h = do
  let hash = Hash $ BH.unpackHash' h
  lift $ putStrLn $ "GET node"
  response :: GRpcReply GetRespP
    <- lift $ gRpcCall @'MsgProtoBuf @DagStore @"DagStore" @"GetNode" client hash
  case response of
    GRpcOk g -> do
      lift $ putStrLn $ "GET: resp len " ++ show (length $ extra_nodes g)
      pure g
    x -> throwError $ "GET: error response was: " ++ show x




put
  :: GrpcClient
  -> NatM (ExceptT String IO) (M BH.Hash) BH.Hash
put client m = do
  let n = toProtoM m
  lift $ putStrLn $ "PUT node"
  response :: GRpcReply Hash
    <- lift $ gRpcCall @'MsgProtoBuf @DagStore @"DagStore" @"PutNode" client n
  case response of
    GRpcOk ph -> liftEither $ fromProtoHash $ Const ph
    x -> throwError $ "PUT: error response was: " ++ show x


getAll
  :: GrpcClient
  -> NatM (ExceptT String IO) BH.Hash (Term M)
getAll c = futaM f
  where
    f :: CVCoalgM (ExceptT String IO) M BH.Hash
    f (Const rh) = do
      gr <- get' c rh
      liftEither . fromProtoGetResp $ Const gr


test :: NatM (ExceptT String IO) (Term M) (Term M)
test input = do
  c <- mkClient
  h <- hcataM (put c) input
  output <- anaM (get c) h
  pure output

test' :: NatM (ExceptT String IO) (Term M) (Term M)
test' input = do
  c <- mkClient
  h <- hcataM (put c) input
  output <- getAll c h
  pure output


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
  :: forall (i :: MTag) x
   . SingI i
  => M (Const x) i
  -> (M (Const Id) i, [(x, Id)])
hashToId m = flip S.runState [] $ hmapM f m
  where
    f :: NatM (S.State [(x, Id)])
               (Const x)
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



-- | Blake2s
-- exact copy of this from the rust side:
--
-- use blake2::Digest;
-- let mut hasher = blake2::Blake2s::new();
-- for link in self.links.iter() {
--     hasher.update(&link.id.0.to_be_bytes());
--     hasher.update(link.hash.0.as_slice());
-- }
-- hasher.update(&self.data.0);
-- let hash = hasher.finalize();
-- Hash(hash)
toCanonicalHash :: Node Identity -> BH.RawBlakeHash
toCanonicalHash n = BH.doHash' $ links ++ [node_data n]
  where
    links = foldMap f (node_links n)
    f h = [ BL.toStrict $ BB.toLazyByteString $ word32LE $ id_data $ runIdentity $ header_id h
          , hash_data $ runIdentity $ header_hash h
          ]



-- ughhhh the haskell one is C so in IO fuck this noise
-- can't use own local hashing alg b/c requires full local tree to hash
-- just downgrade rust end to blake2 lmao fuk


fromProtoHash' :: Hash -> String `Either` BH.RawBlakeHash
fromProtoHash' = maybe (Left "invalid hash") (Right) . BH.bytesToHash . hash_data

fromProtoHash :: NatM (Either String) (Const Hash) BH.Hash
fromProtoHash = fmap Const . fromProtoHash' . getConst


-- -- parse a single node
fromProto
  :: forall (i :: MTag)
   . SingI i
  => Const (Node Identity) i
  -> String `Either` M BH.Hash i
fromProto (Const Node{node_data, node_links}) = do
  let f :: Header Identity -> String `Either` (Id, BH.RawBlakeHash)
      f Header{header_id, header_hash} = do
        let header_id'  = runIdentity header_id
        header_hash' <- fromProtoHash' $ runIdentity header_hash
        pure (header_id', header_hash')

  mappings <- traverse f node_links
  m <- decodeM (sing @i) (BL.fromStrict node_data)
  pure $ idToHash mappings m


type PartialTree f = f (Context f BH.Hash)

-- recursively parse nodes building up a tree of them, TODO: a type for that, term of HEither Hash M
fromProtoGetResp
  :: NatM (Either String) (Const GetRespP) (PartialTree M)
fromProtoGetResp (Const gr) = do
    let unpackNode :: Maybe (Node Maybe)
                   -> String `Either` Node Identity
        unpackNode mn = do
            pn <- maybe (Left "requested node not present") Right $ mn
            n  <- maybe (Left "node fields not present") Right $ requireFields pn
            pure n

        unpackNodeWithHeader :: NodeWithHeaderP
                             -> String `Either` (BH.RawBlakeHash, Node Identity)
        unpackNodeWithHeader mnh = do
            n  <- unpackNode $ node mnh
            mh <- maybe (Left "header not present") Right $ header mnh
            ph <- maybe (Left "header fields not present") Right $ requireHeaderFields mh
            h  <- fromProtoHash' $ runIdentity $ header_hash ph
            pure (h, n)

    node <- (unpackNode $ requested_node gr) >>= fromProto . Const

    -- at this point we DO NOT HAVE TYPE INFO
    -- so CANNOT decode nodes yet, just raw blake hashes and unparsed nodes
    nodes <- traverse unpackNodeWithHeader $ extra_nodes gr

    let deref :: CoalgPartialM (Either String) M BH.Hash
        deref h = do
          case lookup (getConst h) nodes of
            Nothing -> pure $ R h
            Just pn -> do
              nn <- fromProto $ Const pn
              pure $ L nn

    hmapM (anaPartialM deref) node

