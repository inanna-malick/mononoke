{-# LANGUAGE IncoherentInstances #-}

module Merkle.Store.Network where

--------------------------------------------
import           Control.Monad.IO.Class
import           Data.Proxy
import           Servant
import           Servant.Client
--------------------------------------------
import           Merkle.Store
import           Merkle.Types (Hash)
--------------------------------------------


type DerefAPI f = "deref"   :> Capture "hash" (Hash f) :> Get '[JSON] (DerefRes f)
type UploadAPI f = "upload" :> ReqBody '[JSON] (f (Hash f)) :> Post '[JSON] (Hash f)
type StoreAPI f = DerefAPI f :<|> UploadAPI f

-- | Filesystem backed store using the provided dir
netStore
  :: forall f
   . ( Functor f
     , HasClient ClientM (StoreAPI f)
     )
  => Store ClientM f
netStore
  = Store
  { sDeref = derefGet
  , sUploadShallow = uploadPost
  }
  where
    derefGet :<|> uploadPost = client (Proxy :: Proxy (StoreAPI f))

server :: forall f. Store IO f -> Server (StoreAPI f)
server s = derefGet :<|> uploadPost
  where
    derefGet = liftIO . sDeref s
    uploadPost = liftIO . sUploadShallow s

app :: forall f . HasServer (StoreAPI f) '[] => Store IO f -> Application
app s = serve (Proxy :: Proxy (StoreAPI f)) (server s)
