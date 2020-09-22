{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module HGit.Core.Types where

--------------------------------------------
import           Data.Aeson as AE
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import           Data.Functor.Const (Const(..))
import           Data.Functor.Compose
import           Data.List (intersperse)
import           Data.List.NonEmpty (NonEmpty(..), toList)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import           Data.Text.Encoding (decodeLatin1, encodeUtf8)
import           Data.Singletons.TH
import           GHC.Generics
--------------------------------------------
import           HGit.Render.Utils
import           Merkle.Higher.Types hiding (Hash)
import           Merkle.Higher.Store
import           Merkle.Higher.Store.Deref
import           Util.RecursionSchemes as R
import           Util.HRecursionSchemes as HR -- YOLO 420 SHINY AND CHROME
--------------------------------------------

type Hash = Const () -- TODO refactor/rewrite hrecrusion schemes framework stuff


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
  }

data ChangeType a
  = Add (a 'BlobT)
  | Del


fetchLMMT :: NatM m (LMMT m) (M (LMMT m))
fetchLMMT (Term (HC (Tagged _ (HC (Compose m))))) = m

flattenLMMT :: M (LMMT m) :-> M Hash
flattenLMMT = hfmap hashOf

hashOf :: LMMT x :-> Hash
hashOf (Term (HC (Tagged h _))) = h

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
          Add x -> [renderPath _path ++ ":"] ++ getConst x
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

type WIP = HEither Hash `HCompose` M
type WIPT = Term WIP

renderWIPT :: forall m (x :: MTag). SingI x => WIPT x -> [String]
renderWIPT = getConst . hcata f
  where f :: Alg WIP (Const [String])
        f (HC (L hash)) = Const ["hash"]
        f (HC (R m))    = renderM m

-- | hash and lift (TODO: THIS IS NEEDED - dip into merkle lib for refs)
liftLMMT :: forall m x. Applicative m => SingI x => Term M x -> LMMT m x
liftLMMT = hcata f
  where f x = Term $ HC $ Tagged{ _tag = Const (), _elem = HC $ Compose $ pure x}

-- ++ /a/foo foo
-- ++ /a/bar bar
-- ++ /baz baz
-- ++ /.DS_Store jk
commit1 :: Term M 'CommitT
commit1 = Term $ Commit "first commit" changes parents
  where
    changes = [c1, c2, c3, c4]
    c1 = add ("a" :| ["foo"]) . Term $ Blob "foo"
    c2 = add ("a" :| ["bar"]) . Term $ Blob "bar"
    c3 = add ("baz" :| []) . Term $ Blob "baz"
    c4 = add (".DS_Store" :| []) . Term $ Blob "jk"
    parents = Term NullCommit :| []

-- ++ /a/foo foo
-- ++ /a/bar bar
-- ++ /baz baz2
-- ++ /README todo
commit2 :: Term M 'CommitT
commit2 = Term $ Commit "first commit" changes parents
  where
    changes = [c1, c2, c3, c4]
    c1 = add ("a" :| ["foo"]) . Term $ Blob "foo"
    c2 = add ("a" :| ["bar"]) . Term $ Blob "bar"
    c3 = add ("baz" :| [])    . Term $ Blob "baz2"
    c4 = add ("README" :| []) . Term $ Blob "todo"
    parents = Term NullCommit :| []


commit3 :: Term M 'CommitT
commit3 = Term $ Commit "merge" [] parents
  where
    parents = commit1 :| [commit2]


instance ExtractKeys M where
  extractHashKeys (Snapshot tree orig parents) = [unHash tree, unHash orig] ++ fmap unHash parents
  extractHashKeys (File blob lastMod prev) =
    let prev' = fmap unHash prev
     in [unHash blob, unHash lastMod] ++ prev'

  extractHashKeys (Dir children) = unHash . snd <$> Map.toList children
  extractHashKeys NullCommit = []
  extractHashKeys (Commit _ changes parents) =
    let unChangeHash (Add h) = [unHash h]
        unChangeHash  Del    = []
     in (changes >>= (unChangeHash . _change)) ++ toList (fmap unHash parents)
  extractHashKeys (Blob _) = []


instance HFunctor M where
  hfmap f (Snapshot tree orig parents) = Snapshot (f tree) (f orig) (fmap f parents)
  hfmap f (File blob lastMod prev) = File (f blob) (f lastMod) (fmap f prev)
  hfmap f (Dir children) = Dir (fmap f children)
  hfmap f NullCommit = NullCommit
  hfmap f (Commit msg changes parents) =
    let f' Change{..} = case _change of
          Add blob -> Change { _path = _path, _change = Add $ f blob}
          Del -> Change { _path = _path, _change = Del}
     in Commit msg (fmap f' changes) (fmap f parents)
  hfmap f (Blob x) = Blob x

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
