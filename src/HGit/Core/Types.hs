{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module HGit.Core.Types where


import Data.Aeson.GADT.TH
import Data.Aeson as AE
import GHC.Generics
--------------------------------------------
import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Data.Functor.Const (Const(..))
import qualified Data.ByteString.Lazy as LB
import           Data.Functor.Compose
import           Data.List (intersperse)
import           Data.List.NonEmpty (NonEmpty(..), toList)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Singletons.TH
import qualified Data.Text as T
--------------------------------------------
import           Merkle.Types.BlakeHash
import           HGit.Render.Utils
import           Util.HRecursionSchemes as HR -- YOLO 420 SHINY AND CHROME
--------------------------------------------


-- TODO refactor/rewrite hrecrusion schemes framework stuff - this is from other pkg
type Hash = Const RawBlakeHash

$(singletons [d|
  data MTag = SnapshotT | FileTree | CommitT | BlobT
 |])

type Path = String -- TODO use Text

add :: NonEmpty Path -> a 'BlobT -> Change a
add p a = Change { _path = p, _change = Add a}

del :: NonEmpty Path -> Change a
del p = Change { _path = p, _change = Del}

data Change a
  = Change
  { _path::   NonEmpty Path
  , _change:: ChangeType a
  } deriving (Generic)

instance ToJSON (a 'BlobT) => ToJSON (Change a) where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON (a 'BlobT) => FromJSON (Change a)

data ChangeType a
  = Add (a 'BlobT)
  | Del
 deriving (Generic)

instance ToJSON (a 'BlobT) => ToJSON (ChangeType a) where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON (a 'BlobT) => FromJSON (ChangeType a)

data M a i where
  -- snapshots:
  Snapshot
    :: a 'FileTree    -- snapshot of file tree in commit
    -> a 'CommitT     -- originating commit
    -> [a 'SnapshotT] -- parent snapshots, if any
    -- NOTE: ^^ added this to make algorithm for snapshot gen prettier
    -- NOTE: ^^ valid choice according to rain but bad motivation? idk
    -> M a 'SnapshotT

  -- file tree entries:
  File
    :: a 'BlobT    -- file blob
    -> a 'CommitT  -- last modified in this commit
    -> [a 'FileTree] -- previous incarnation(s)
    -> M a 'FileTree

  Dir
    --  TODO: will need canonical on-disk map repr/cannonical hash
    :: Map Path (a 'FileTree) -- children
 -- Q: do I store this here, too, or do I only store it for files?
 --   -> a 'CommitT -- last modified in this commit
 --   -> a 'FileTree -- previous incarnation
    -> M a 'FileTree

  -- commits:
  NullCommit
    :: M a 'CommitT

  Commit
    :: String                -- commit message (TODO text)
    -> [Change a]            -- list of inline changes
    -> NonEmpty (a 'CommitT) -- parent commits
    -> M a 'CommitT

  -- blobs:
  Blob
    :: String -- TODO: use bytestring, currently string to simplify experimentations
    -> M a 'BlobT

deriveJSONGADT ''M




-- CANNONICAL HASH FN, i guess (TODO: better?)
hashM :: M Hash :-> Hash
hashM = Const . doHash' . pure . LB.toStrict . encode






-- | algebra, assumes all sub-entities have been rendered down to a list of lines
renderM :: M (Const [String]) i -> Const [String] i
renderM (Snapshot tree orig parents)
  = Const $  mconcat [["Snapshot:"]] ++
  ( indent $
           [ getConst tree
           , getConst orig
           , ["parents:"] ++ (indent $ (getConst <$> parents))
           ]
  )
renderM (File blob lastMod prev)
  = Const $ mconcat [["File:"]] ++
  ( indent $
           [ getConst blob
           , getConst lastMod
           ] ++ fmap getConst prev
  )
renderM (Dir children)
  = let children' = (\(k,v) -> [k ++ ": "] ++ getConst v) <$> Map.toList children
     in Const $ mconcat [["Dir:"]] ++ indent children'
renderM NullCommit = Const ["NullCommit"]
renderM (Commit msg changes parents)
  = let renderPath = mconcat . intersperse "/" . toList
        renderChange Change{..} = case _change of
          Del   -> [renderPath _path ++ ": Del"]
          Add x -> [renderPath _path ++ ": Add: "] ++ getConst x
     in Const $ mconcat [["Commit \"" ++ msg ++ "\""]] ++
  ( indent $
           [ ["changes:"] ++ (indent $ (renderChange <$> changes))
           , ["parents:"] ++ (indent $ (getConst <$> toList parents))
           ]
  )
renderM (Blob x) = Const ["Blob: " ++ x]

-- TODO: can't hcatM LMMT b/c 'm' is in the stack that needs to be HTraversable
renderLMMT :: forall m (x :: MTag). SingI x => Monad m => LMMT m x -> m [String]
renderLMMT = getConst . hcata f
  where f :: Alg (LMM m) (Const (m [String]))
        f (HC (Tagged _ (HC (Compose m)))) = Const $ do
            m' <- m
            m'' <- hmapM (\x -> Const <$> getConst x) m'
            pure $ getConst $ renderM m''

showLMMT :: SingI x => LMMT IO x -> IO ()
showLMMT = (>>= const (pure ())) . (>>= traverse putStrLn) . renderLMMT

-- Lazy Merkle M
type LMM m = Tagged Hash `HCompose` Compose m `HCompose` M
type LMMT m = Term (LMM m)


fetchLMMT :: Functor m => NatM m (LMMT m) ((Tagged Hash `HCompose` M) (LMMT m))
fetchLMMT (Term (HC (Tagged h (HC (Compose m))))) = HC . Tagged h <$> m

flattenLMMT :: M (LMMT m) :-> M Hash
flattenLMMT = hfmap hashOfLMMT

hashOfLMMT :: LMMT m :-> Hash
hashOfLMMT (Term (HC (Tagged h _))) = h

type WIP m = HEither (LMMT m) `HCompose` Tagged Hash `HCompose` M
type WIPT m = Term (WIP m)

uploadWIPT
  :: forall m
   . Monad m
  => NatM m (M Hash) Hash
  -> NatM m (WIPT m) (LMMT m)
uploadWIPT upload (Term (HC (L lmmt))) = pure lmmt
uploadWIPT upload (Term (HC (R (HC (Tagged h m))))) = do
  lmmt <- hmapM (uploadWIPT upload) m
  h' <- upload $ hfmap hashOfLMMT lmmt
  -- TODO assert h == h'
  pure $ Term $ HC $ Tagged h' $ HC $ Compose $ pure lmmt

fetchWIPT :: Applicative m => NatM m (WIPT m) ((Tagged Hash `HCompose` M) (WIPT m))
fetchWIPT (Term (HC (L lmmt))) = hfmap unmodifiedWIP <$> fetchLMMT lmmt
fetchWIPT (Term (HC (R hct))) = pure hct

hashOfWIPT :: WIPT m :-> Hash
hashOfWIPT (Term (HC (L lmmt))) = hashOfLMMT lmmt
hashOfWIPT (Term (HC (R (HC (Tagged h _))))) = h

unmodifiedWIP :: LMMT m :-> WIPT m
unmodifiedWIP = Term . HC . L

modifiedWIP :: M (WIPT m) :-> WIPT m
modifiedWIP m = Term . HC . R . HC $ Tagged h m
  where
    h = hashM $ hfmap hashOfWIPT m


showHash :: forall (i :: MTag). SingI i => Hash i -> String
showHash h =
  let h' = take 6 $ T.unpack $ hashToText $ getConst h
   in "[" ++ typeTagName (sing :: Sing i) ++ ":" ++ h' ++ "]"

typeTagName :: forall (i :: MTag). Sing i -> String
typeTagName s = case s of
  SSnapshotT -> "snapshot"
  SFileTree  -> "filetree"
  SCommitT   -> "commit"
  SBlobT     -> "blob"


renderWIPT :: forall m (x :: MTag). SingI x => WIPT m x -> [String]
renderWIPT = getConst . hcata f
  where f :: Alg (WIP m) (Const [String])
        f (HC (L lmmt)) = let h = showHash $ hashOfLMMT lmmt
                           in Const [h]
        f (HC (R (HC (Tagged _ m)))) = renderM m


renderWIPTM :: forall m (x :: MTag). SingI x => Monad m => WIPT m x -> m [String]
renderWIPTM = getConst . hcata f
  where f :: Alg (WIP m) (Const (m [String]))
        f (HC (L lmmt)) = Const $ renderLMMT lmmt
        f (HC (R (HC (Tagged _ m)))) = Const $ do -- TODO: include hash in output
            m' <- hmapM (\x -> Const <$> getConst x) m
            pure $ getConst $ renderM m'

-- | hash and lift (TODO: THIS IS NEEDED - dip into merkle lib for refs)
liftLMMT :: forall m x. Applicative m => SingI x => Term M x -> LMMT m x
liftLMMT = hcata f
  where
    f x = Term $ HC $ Tagged{ _tag = h x, _elem = HC $ Compose $ pure x}
    h x = hashM $ hfmap hashOfLMMT x


expandHash :: forall m. Monad m => StoreRead m -> Hash :-> (LMMT m)
expandHash get = ana f
  where
    f :: Coalg (LMM m) Hash
    f h = HC $ Tagged h $ HC $ Compose $ do
      (HC (Compose mmh)) <- get h
      case mmh of
        Nothing -> error "broken link in STM storage"
        Just mh -> pure mh


uploadM :: Monad m => StoreWrite m -> NatM m (Term M) Hash
uploadM upload = hcataM upload


-- ++ /a/foo foo
-- ++ /a/bar bar
-- ++ /baz baz
commit0 :: Term M 'CommitT
commit0 = Term $ Commit "c0: first commit" changes parents
  where
    changes = [c1, c2, c3]
    c1 = add ("a" :| ["foo"]) . Term $ Blob "foo"
    c2 = add ("a" :| ["bar"]) . Term $ Blob "bar"
    c3 = add ("baz" :| []) . Term $ Blob "baz"
    parents = Term NullCommit :| []

-- ++ /.DS_Store jk
-- -- /a/bar
commit1 :: Term M 'CommitT
commit1 = Term $ Commit "c1: askdfj" changes parents
  where
    changes = [c1, c2]
    c1 = add (".DS_Store" :| []) . Term $ Blob "jk"
    c2 = del ("a" :| ["bar"])
    parents = commit0 :| []

-- ++ /README todo
commit2 :: Term M 'CommitT
commit2 = Term $ Commit "c2: todo: readme" changes parents
  where
    changes = [c1]
    c1 = add ("README" :| []) . Term $ Blob "todo"
    parents = commit0 :| []

-- TODO: should this have 'bar', deleted in 1/2 of parent commits and not changed in this commit?
-- TODO: I think that should have to be resolved (file present in one, absent in other - maybe?)
commit3 :: Term M 'CommitT
commit3 = Term $ Commit "c3: merge" [resolvingChange] parents
  where
    parents = commit1 :| [commit2]
    resolvingChange = add ("baz" :| []) . Term $ Blob "baz3"


-- instance ExtractKeys M where
--   extractHashKeys (Snapshot tree orig parents) = [unHash tree, unHash orig] ++ fmap unHash parents
--   extractHashKeys (File blob lastMod prev) =
--     let prev' = fmap unHash prev
--      in [unHash blob, unHash lastMod] ++ prev'

--   extractHashKeys (Dir children) = unHash . snd <$> Map.toList children
--   extractHashKeys NullCommit = []
--   extractHashKeys (Commit _ changes parents) =
--     let unChangeHash (Add h) = [unHash h]
--         unChangeHash  Del    = []
--      in (changes >>= (unChangeHash . _change)) ++ toList (fmap unHash parents)
--   extractHashKeys (Blob _) = []


instance HFunctor M where
  hfmap f (Snapshot tree orig parents) = Snapshot (f tree) (f orig) (fmap f parents)
  hfmap f (File blob lastMod prev) = File (f blob) (f lastMod) (fmap f prev)
  hfmap f (Dir children) = Dir (fmap f children)
  hfmap _ NullCommit = NullCommit
  hfmap f (Commit msg changes parents) =
    let f' Change{..} = case _change of
          Add blob -> Change { _path = _path, _change = Add $ f blob}
          Del -> Change { _path = _path, _change = Del}
     in Commit msg (fmap f' changes) (fmap f parents)
  hfmap _ (Blob x) = Blob x

instance HTraversable M where
  hmapM f (Snapshot tree orig parents) = do
    tree' <- f tree
    orig' <- f orig
    parents' <- traverse f parents
    pure $ Snapshot tree' orig' parents'
  hmapM f (File blob lastMod prev) = do
    blob' <- f blob
    lastMod' <- f lastMod
    prev' <- traverse f prev
    pure $ File blob' lastMod' prev'
  hmapM f (Dir children) = do
    children' <- traverse f children
    pure $ Dir children'
  hmapM _ NullCommit = pure NullCommit
  hmapM f (Commit msg changes parents) =
    let f' Change{..} = case _change of
          Add blob -> do
            blob' <- f blob
            pure $ Change { _path = _path, _change = Add blob'}
          Del -> pure $ Change { _path = _path, _change = Del}
     in do
      changes' <- traverse f' changes
      parents' <- traverse f parents
      pure $ Commit msg changes' parents'
  hmapM _ (Blob x) = pure $ Blob x







-- TODO new module

data BlobStore
  = BlobStore
  { snapshotBS :: (Map (Hash 'SnapshotT) (M Hash 'SnapshotT))
  , fileTreeBS :: (Map (Hash 'FileTree)  (M Hash 'FileTree))
  , commitBS   :: (Map (Hash 'CommitT)   (M Hash 'CommitT))
  , blobBS     :: (Map (Hash 'BlobT)     (M Hash 'BlobT))
  }

emptyBlobStore :: BlobStore
emptyBlobStore
  = BlobStore
  { snapshotBS = Map.empty
  , fileTreeBS = Map.empty
  , commitBS   = Map.empty
  , blobBS     = Map.empty
  }

-- (sing :: Sing i)
getBlobStore :: forall (i :: MTag). Sing i -> Hash i -> BlobStore -> Maybe (M Hash i)
getBlobStore s h bs = case s of
  SSnapshotT -> Map.lookup h $ snapshotBS bs
  SFileTree  -> Map.lookup h $ fileTreeBS bs
  SCommitT   -> Map.lookup h $ commitBS   bs
  SBlobT     -> Map.lookup h $ blobBS     bs


putBlobStore :: forall (i :: MTag). Sing i -> Hash i -> M Hash i -> BlobStore -> BlobStore
putBlobStore s h m bs = case s of
  SSnapshotT -> bs { snapshotBS = Map.insert h m $ snapshotBS bs }
  SFileTree  -> bs { fileTreeBS = Map.insert h m $ fileTreeBS bs }
  SCommitT   -> bs { commitBS   = Map.insert h m $ commitBS   bs }
  SBlobT     -> bs { blobBS     = Map.insert h m $ blobBS     bs }


type StoreRead  m = NatM m Hash ((Compose Maybe `HCompose` M) Hash)
type StoreWrite m = NatM m (M Hash) Hash

data Store m
  = Store
  { sRead  :: StoreRead  m
  , sWrite :: StoreWrite m
  }


stmIOStore :: MonadIO m => TVar BlobStore -> Store m
stmIOStore tvar
  = let store' = stmStore tvar
     in Store
  { sRead = \h   -> liftIO $ atomically $ sRead store' h
  , sWrite = \mh -> liftIO $ atomically $ sWrite store' mh
  }




stmStore :: TVar BlobStore -> Store STM
stmStore tvar
  = Store
  { sRead = \h -> do
      bs <- readTVar tvar
      pure $ HC . Compose $ getBlobStore sing h bs
  , sWrite = \mh -> do
      let h = hashM mh
      modifyTVar tvar $ \bs ->
        putBlobStore sing h mh bs
      pure h
  }



