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

module HGit.Client where

import           Data.Word (Word64)
import qualified Data.Text               as T
import           GHC.Generics
import           Mu.Quasi.GRpc
import           Mu.Schema
import Mu.GRpc.Client.Optics
import           Mu.GRpc.Client.TyApps
import qualified Data.ByteString as B

grpc "GRPCStore" id "dagstore.proto"

-- data Id
--   = Id { id :: B.ByteString }
--   deriving (Eq, Show, Generic)

-- instance ToSchema   GRPCStore "Id" Id
-- instance FromSchema GRPCStore "Id" Id


-- data Hash
--   = Hash { hash :: B.ByteString }
--   deriving (Eq, Show, Generic)

-- instance ToSchema   GRPCStore "Hash" Hash
-- instance FromSchema GRPCStore "Hash" Hash

-- data Header
--   = Header { id :: Id, hash :: Hash, size :: Word64 }
--   deriving (Eq, Show, Generic)

-- instance ToSchema   GRPCStore "Header" Header
-- instance FromSchema GRPCStore "Header" Header

-- data Node
--   = Node { data :: B.ByteString, hash :: Hash, size :: Word64 }
--   deriving (Eq, Show, Generic)

-- instance ToSchema   GRPCStore "Header" Header
-- instance FromSchema GRPCStore "Header" Header

-- message Node {
--   bytes data = 1;
--   repeated Header links = 2;
-- }

main :: IO ()
main = do
    let config = grpcClientConfigSimple "127.0.0.1" 8080 False
    Right (client) <- setup config
    get client
  -- where setup config = setupGrpcClient' config
  where setup config = initGRpc config msgProtoBuf

get :: GRpcConnection DagStore 'MsgProtoBuf -> IO ()
get client = do
  let req = record (B.empty, [])
  putStrLn $ "PUT node"
  response <- client ^. #putNode $ req
  -- response :: GRpcReply String
  --   <- gRpcCall @'MsgProtoBuf @DagStore @"DagStore" @"PutNode" client req
  putStrLn $ "PUT: response was: " ++ show response
