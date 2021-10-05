module Merkle.Generic.BlakeHash where

--------------------------------------------
import           Control.Applicative (Const(..))
import qualified Crypto.Hash as CH
import qualified Crypto.Hash.Algorithms as CHA
import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as AE
import qualified Data.ByteArray as BA
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import           Data.Text (Text, unpack, pack)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
--------------------------------------------


type Hash = Const RawBlakeHash

emptyHash :: forall x. Hash x
emptyHash = doHash [mempty]

-- | Hash pointer (points to value from which hash was derived)
-- , digests tagged with hash alg (blake2b_256) at type level
newtype RawBlakeHash = RawBlakeHash { unRawBlakeHash :: CH.Digest CHA.Blake2s_256 }
  deriving (Eq, Ord)


hashToText :: RawBlakeHash -> Text
hashToText = decodeUtf8 . B16.encode . BA.pack . BA.unpack . unRawBlakeHash

textToHash :: Text -> Maybe RawBlakeHash
textToHash = fmap RawBlakeHash . f . B16.decode . encodeUtf8
  where
    f (x, remainder)
      -- no remaining unparseable bytes, sucess (note, 2x failure paths, todo: better logging)
      | remainder == B.empty = CH.digestFromByteString x
      | otherwise = Nothing

bytesToHash :: ByteString -> Maybe RawBlakeHash
bytesToHash = fmap RawBlakeHash . CH.digestFromByteString

instance Show RawBlakeHash where
  show x = "#[" ++ unpack (hashToText x) ++ "]"


instance AE.ToJSONKey RawBlakeHash where
  toJSONKey = AE.toJSONKeyText hashToText

instance AE.FromJSONKey RawBlakeHash where
  fromJSONKey = AE.FromJSONKeyTextParser (maybe (fail "parsing failed") pure . textToHash)


instance AE.ToJSON RawBlakeHash where
  toJSON = AE.String . hashToText

instance AE.FromJSON RawBlakeHash where
  parseJSON =
    AE.withText "RawBlakeHash"
      (maybe (fail "parsing failed") pure . textToHash)

unpackString :: String -> ByteString
unpackString = encodeUtf8 . pack

unpackHash :: Hash x -> ByteString
unpackHash = unpackHash' . getConst

unpackHash' :: RawBlakeHash -> ByteString
unpackHash' = BA.pack . BA.unpack . unRawBlakeHash

-- | do actual hash computation type stuff. blake2b!
doHash :: [ByteString] -> Hash i
doHash = Const . doHash'

doHash' :: [ByteString] -> RawBlakeHash
doHash' = RawBlakeHash . CH.hashFinalize
       . CH.hashUpdates (CH.hashInit :: CH.Context CHA.Blake2s_256)
