
module HGit.Types.RepoState (RepoState(..), initialRepoState) where

--------------------------------------------
import           Data.Aeson
import           Data.Text (Text)
import           GHC.Generics
--------------------------------------------
import           HGit.Serialization (hash)
import           HGit.Types.Common
import           HGit.Types.Merkle
--------------------------------------------
import qualified Data.Functor.Compose as FC
import           Data.Functor.Identity
import           Util.HRecursionSchemes -- YOLO 420 SHINY AND CHROME
import           Util.MyCompose


data RepoState
  = RepoState
  { branches      :: [(BranchName, HashPointer)]
  , currentBranch :: BranchName
  , substantiated :: SubstantiationState
  } deriving (Generic)

initialRepoState :: RepoState
initialRepoState
  = RepoState
  { branches      = [(initial, hash NullCommit)]
  , currentBranch = initial
  , substantiated = SubstantiationState
    ( hash emptyRoot
    , emptyRoot
    )
  }
  where
    emptyRoot = TopLevelDir []
    initial = "default"


-- for diffing: just traverse this and, for each substantiated path, ingest and compare file
-- note: this will end up being a big ass object if each file is substantiated - can't force
-- via type level but MUST ENFORCE that no file blobs are represented inline here, just dir structure
-- can grab file blobs from store if required for diffing
newtype SubstantiationState
  = SubstantiationState
  { unSubstantiationState :: (HashPointer, HGit (Term (FC.Compose HashIndirect :++ HGit)) 'TopLevelDirTag)
  }

instance ToJSON SubstantiationState where
    toJSON (SubstantiationState (p, TopLevelDir children)) =
        object [ "pointer"  .= p
               , "type"     .= ("topleveldir" :: Text)
               , "children" .= fmap SubstantiationState' children
               ]

instance FromJSON SubstantiationState where
    parseJSON = withObject "SubstantiationState" $ \v -> do
        p <- v .:  "pointer"
        -- TODO: some kind of type level thing here to link phantom type and string tag
        typ     <- v .: "type"
        case typ of
          "topleveldir" -> do
              children <- fmap unSubstantiationState' <$> v .: "children"
              pure $ SubstantiationState (p, TopLevelDir children)
          x -> fail $ "require [topleveldir] type" ++ x

newtype SubstantiationState'
  = SubstantiationState'
  { unSubstantiationState' :: Term (FC.Compose HashIndirect :++ HGit) 'DirTag
  }

instance ToJSON SubstantiationState' where
    toJSON (SubstantiationState' (Term (HC (FC.Compose (C (p, x)))))) = case x of
      Nothing ->
        object ["pointer" .= p]
      Just (Dir path children) ->
        object [ "pointer"  .= p
               , "path"     .= path
               , "type"     .= ("dir" :: Text)
               , "children" .= fmap SubstantiationState' children
               ]
      Just (File path child) ->
        object [ "pointer"     .= p
               , "path"        .= path
               , "type"        .= ("file" :: Text)
               -- NOTE: will not round trip if file body is substantiated, but this is correct
               , "bodyPointer" .= pointer child
               ]


instance FromJSON SubstantiationState' where
    parseJSON = fmap (fmap (SubstantiationState' . Term . HC . FC.Compose . C))
            <$> withObject "SubstantiationState'" $ \v -> do
        p <- v .:  "pointer"
        -- TODO: some kind of type level thing here to link phantom type and string tag
        typ     <- v .:? "type"
        case typ of
          Nothing ->
            pure (p, Nothing)
          Just "dir" -> do
              children <- fmap unSubstantiationState' <$> v .: "children"
              path     <- v .: "path"
              pure (p, Just $ Dir path children)
          Just "file" -> do
              bodyPointer <- v .: "bodyPointer"
              path        <- v .: "path"
              let bodyPointer' = Term . HC . FC.Compose $ C (bodyPointer, Nothing)
              pure (p, Just $ File path bodyPointer')
          Just x -> fail $ "require [file, dir] type" ++ x



instance ToJSON RepoState where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON RepoState
