{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module HGit.Serialization where

--------------------------------------------
import           Data.Aeson
import           Data.Aeson.Types (Parser)
import           Data.Functor.Compose
import qualified Data.Hashable as H
import           Data.Singletons
import           Data.Text
import           Data.Vector (fromList, toList)
--------------------------------------------
import           HGit.Types.HGit
import           Merkle.Types
import           Util.HRecursionSchemes
import           Util.MyCompose
--------------------------------------------

{-
ideas for lazy fetch
- file/dir pointer fs entities (fname.dir.pointer/fname.file.pointer w/ pointer as contents)
- each dir has a .hgit-substantiation.json file with this structure
-- SubstantiationState = Substantiated (HashPointer, FilePath) | UnSubstantiated Pointer
-- actually, [NamedFilePointer (Const (HashPointer, Maybe FilePath)] will work
--   idea is that on (lazy/strict) checkout this gets populated with the file paths and hashes
--   for all fetched files, and on read this gets read and consulted - can also be used for 'status'
--   style fsstate vs. repo pointer diff - but actually that's bad, ideally logic gets used once..
-}

sdecode :: NatM Parser (Const Value) (HGit (Const HashPointer))
sdecode = sdecode' sing . getConst

sdecode' :: Sing x -> Value -> Parser $ HGit (Const HashPointer) x
sdecode' = \case
  SFileChunkTag -> withObject "HGit (Const HashPointer) FileChunkTag" $ \v -> do
        typ  <- v .: "type"
        case typ of
          "blobtree" -> do
              children <- v .: "children"
              pure $ BlobTree $ fmap (Const . HashPointer) children
          "blob" -> do
              contents <- v .: "contents"
              pure $ Blob contents
          x -> fail $ "require [blob, blobtree] type" ++ x

  SDirTag       -> withObject "HGit (Const HashPointer) DirTag" $ \v -> do
        typ  <- v .: "type"
        case typ of
          "dir" -> do
              children  <- v .: "children"
              children' <- traverse parseThingy children
              pure $ Dir children'
          x -> fail $ "require [file, dir] type" ++ x

  SCommitTag    -> withObject "HGit (Const HashPointer) CommitTag" $ \v -> do
        typ  <- v .: "type"
        case typ of
          "nullcommit" -> pure NullCommit
          "commit" -> do
              name <- v .: "name"
              root <- v .: "root"
              parents <- v .: "parents"
              pure $ Commit name (Const $ HashPointer root) (fmap (Const . HashPointer) parents)
          x -> fail $ "require [commit, nullcommit] type" ++ x

  where
    parseThingy
      :: Value
      -> Parser $ NamedFileTreeEntity (Const HashPointer)
    parseThingy = decodeNamedDir -- both branches just Const pointers
      (fmap (Const . HashPointer) . (.: "pointer")) (fmap (Const . HashPointer) . (.: "pointer"))

encodeNamedDir
  :: (f 'DirTag       -> [(Text, Value)])
  -> (f 'FileChunkTag -> [(Text, Value)])
  -> NamedFileTreeEntity f
  -> Value
encodeNamedDir ed ef (path, e)
  = object $
  [ "path" .= path
  ] ++ case e of
        DirEntity  dir  -> ["type" .= ("dir" :: Text)] ++ ed dir
        FileEntity file -> ["type" .= ("file" :: Text)] ++ ef file

decodeNamedDir
  :: (Object -> Parser (f 'DirTag))
  -> (Object -> Parser (f 'FileChunkTag))
  -> Value
  -> Parser (NamedFileTreeEntity f) -- lmao, need new type
decodeNamedDir pd pf
  =  withObject "named dir entity pointer thingy" $ \v -> do
        name    <- v .: "path"
        typ     <- v .: "type"
        case typ of
          "dir"  -> do
            e <- pd v
            pure (name, DirEntity e)
          "file" -> do
            e <- pf v
            pure (name, FileEntity e)
          x      -> fail $ "require [file, dir] type" ++ x

sencode :: HGit (Const HashPointer) :-> Const Value
sencode x =  Const $ case x of
    Blob contents ->
        object [ "type"     .= ("blob" :: Text)
               , "contents" .= pack contents
               ]
    BlobTree children ->
        object [ "type"     .= ("blobtree" :: Text)
               , "children" .= fmap (unHashPointer . getConst) children
               ]

    Dir children ->
        object [ "type" .= ("dir" :: Text)
               , "children" .= fmap mkThingy children
               ]

    Commit name root parents ->
        object [ "type" .= ("commit" :: Text)
               , "name" .= pack name
               , "root" .= (unHashPointer $ getConst root)
               , "parents" .= fmap (unHashPointer . getConst) parents
               ]
    NullCommit ->
        object [ "type" .= ("nullcommit" :: Text)
               ]
  where
    mkThingy :: NamedFileTreeEntity (Const HashPointer) -> Value
    mkThingy = encodeNamedDir (pure . ("pointer" .=) . unHashPointer . getConst)
                              (pure . ("pointer" .=) . unHashPointer . getConst)

structuralHash :: HGit (Const HashPointer) :-> Const HashPointer
-- special cases
structuralHash (Dir []) = emptyDirHash
structuralHash (NullCommit) = nullCommitHash

-- file-type entities
structuralHash (Blob x) = Const $ mkHashPointer $ H.hash x
structuralHash (BlobTree xs) = Const $ mkHashPointer $ H.hash xs

-- non-empty dir-type entities
structuralHash (Dir xs) = Const $ mkHashPointer $ H.hash $ fmap hashNFTE xs
  where hashNFTE (name, f) = H.hash name `H.hashWithSalt` hashFTE f
        hashFTE =
          fte (\chp -> H.hash ("file" :: Text) `H.hashWithSalt` H.hash chp)
              (\chp -> H.hash ("dir"  :: Text) `H.hashWithSalt` H.hash chp)

-- commit-type entities
structuralHash (Commit msg root parents)
  = Const $ mkHashPointer $ H.hash
  [ H.hash msg
  , H.hash root
  , H.hash parents
  ]

nullCommitHash :: Const HashPointer 'CommitTag
nullCommitHash = Const $ mkHashPointer 0

emptyDirHash :: Const HashPointer 'DirTag
emptyDirHash = Const $ mkHashPointer 0

newtype HashIndirectTerm i
  = HashIndirectTerm
  { unHashIndirectTerm :: Term (HashIndirect HGit) i
  }

-- NOTE: this really needs round trip properties for surety.. which means generators..
instance SingI i => FromJSON (HashIndirectTerm i) where
    parseJSON = fmap HashIndirectTerm . anaM alg . Const
      where
        -- parser (Result) is a monad, so we can just run in that
        alg :: CoalgM Parser (HashIndirect HGit) (Const Value)
        alg x = flip (withObject "pointer tagged entity") (getConst x) $ \o -> do
          p <- o .:  "pointer"
          (mentity :: Maybe Value) <- o .:! "entity" -- entity present but null != entity not present
          case mentity of
            Nothing -> pure $ Pair p $ HC $ Compose Nothing
            Just entity -> handle p entity

        handle :: forall i'
                . SingI i'
               => Const HashPointer i'
               -> Value
               -> Parser $ (HashIndirect HGit) (Const Value) i'
        handle p v = case sing @i' of
          SDirTag -> handleDirTag p v -- todo inline
          SCommitTag -> case v of
            Null -> pure $ Pair p $ HC $ Compose $ Just $ NullCommit
            x    -> flip (withObject "commit") x $ \o -> do
              name <- o .: "msg"
              root <- o .: "root"
              parents <- o .: "parents"
              -- pure $ HC $ Compose $ C (p, Just $ Commit name (Const root) (fmap Const parents))
              pure $ Pair p $ HC $ Compose $ Just $ Commit name (Const root) (fmap Const parents)


          SFileChunkTag -> case v of
            (String t) -> pure $ Pair p $ HC $ Compose $ Just $ Blob $ unpack t
            (x :: Value) -> flip (withArray "blobtree entries") x $ \a ->
              -- pure $ HC $ Compose $ C (p, Just $ BlobTree $ fmap Const $ toList a)
              pure $ Pair p $ HC $ Compose $ Just $ BlobTree $ fmap Const $ toList a


        handleDirTag
          :: Const HashPointer 'DirTag
          -> Value -> Parser $ (HashIndirect HGit) (Const Value) 'DirTag
        handleDirTag p = withArray "dir entries" $ \a -> do
          (elems :: [NamedFileTreeEntity (Const Value)]) <- traverse mkElem $ toList a
          let res :: (HashIndirect HGit) (Const Value) 'DirTag
              res = Pair p $ HC $ Compose $ Just $ Dir elems
          pure $ res

        mkElem :: Value -> Parser $ NamedFileTreeEntity (Const Value)
        mkElem v = decodeNamedDir handleDir handleFile v
        handleFile o = do
          file <- o .: "file"
          pure $ Const file
        handleDir o = do
          dir <- o .: "dir"
          pure $ Const dir

instance SingI i => ToJSON (HashIndirectTerm i) where
    toJSON = getConst . cata alg . unHashIndirectTerm
      where
        alg :: Alg (HashIndirect HGit) (Const Value)
        alg (Pair (Const p) (HC (Compose Nothing))) = Const $ object ["pointer" .= unHashPointer p]
        alg (Pair (Const p) (HC (Compose (Just x)))) = Const $
          object [ "pointer" .= unHashPointer p
                 , "entity" .= encodeEntity  x
                 ]

        encodeEntity :: HGit (Const Value) :=> Value
        encodeEntity (Dir xs) = Array $ fromList $ fmap (encodeNamedDir (pure . ("file" .= )) (pure . ("dir" .= ))) xs

        encodeEntity (NullCommit) = Null
        encodeEntity (Commit msg root parents) =
          object [ "msg" .= msg
                 , "root" .= getConst root
                 , "parents" .= fmap getConst parents
                 ]

        encodeEntity (Blob fc) = String $ pack fc
        encodeEntity (BlobTree fcs) = Array $ fmap getConst $ fromList fcs
