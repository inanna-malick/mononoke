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


data M a i where
  -- snapshots:
  Snapshot
    :: a 'FileTree -- snapshot of file tree in commit
    -> a 'CommitT -- originating commit
    -> M a 'SnapshotT

  -- file tree entries:
  File
    :: a 'BlobT     -- file blob
    -> a 'CommitT  -- last modified in this commit
    -> a 'FileTree -- previous incarnation
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
    :: String           -- commit message (TODO text)
    -> NonEmpty (Change a)  -- list of inline changes
    -> NonEmpty (a 'CommitT) -- parent commits
    -> M a 'CommitT

  -- blobs:
  Blob
    :: String -- TODO: use bytestring, currently string to simplify experimentations
    -> M a 'BlobT




-- | indent and concat
-- [["a", "b"], ["c"], ["d"], []] results in
--
-- ├── a
-- │   b
-- ├── c
-- └── d
indent :: [[String]] -> [String]
indent xs =
  let apply _ _ [] = []
      apply f g (x:xs) = f x : fmap g xs
      removeEmpty = filter (not . null)
    in mconcat $ reverse $ apply
         (apply ("└── " ++) ("│   " ++))
         (apply ("├── " ++) ("│   " ++))
         (reverse $ removeEmpty xs)

-- | algebra, assumes all sub-entities have been rendered down to a list of lines
renderM :: M (Const [String]) i -> Const [String] i
renderM (Snapshot tree orig)
  = Const $  mconcat [["Snapshot:"]] ++
  ( indent $
           [ getConst tree
           , getConst orig
           ]
  )
renderM (File blob lastMod prev)
  = Const $ mconcat [["File:"]] ++
  ( indent $
           [ getConst blob
           , getConst lastMod
           , getConst prev
           ]
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
           [ ["changes:"] ++ (indent $ (renderChange <$> toList changes))
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

-- Lazy Merkle M
type LMM m = Tagged Hash `HCompose` Compose m `HCompose` M
type LMMT m = Term (LMM m)

-- | hash and lift (TODO: THIS IS NEEDED - dip into merkle lib for refs)
liftLMMT :: forall m x. Applicative m => SingI x => Term M x -> LMMT m x
liftLMMT = hcata f
  where f x = Term $ HC $ Tagged{ _tag = Const (), _elem = HC $ Compose $ pure x}

-- ++ /a/foo foo
-- ++ /a/bar bar
-- ++ /baz baz
-- ++ /.DS_Store jk
commit1 :: Applicative m => LMMT m 'CommitT
commit1 = liftLMMT  . Term $ Commit "first commit" changes parents
  where
    changes = c1 :| [c2, c3, c4]
    c1 = add ("a" :| ["foo"]) . Term $ Blob "foo"
    c2 = add ("a" :| ["bar"]) . Term $ Blob "bar"
    c3 = add ("baz" :| []) . Term $ Blob "baz"
    c4 = add (".DS_Store" :| []) . Term $ Blob "jk"
    parents = Term NullCommit :| []

-- ++ /a/foo foo
-- ++ /a/bar bar
-- ++ /baz baz2
-- ++ /README todo
commit2 :: Applicative m => LMMT m 'CommitT
commit2 = liftLMMT  . Term $ Commit "first commit" changes parents
  where
    changes = c1 :| [c2, c3, c4]
    c1 = add ("a" :| ["foo"]) . Term $ Blob "foo"
    c2 = add ("a" :| ["bar"]) . Term $ Blob "bar"
    c3 = add ("baz" :| [])    . Term $ Blob "baz2"
    c4 = add ("README" :| []) . Term $ Blob "todo"
    parents = Term NullCommit :| []



instance ExtractKeys M where
  extractHashKeys (Snapshot tree orig) = [unHash tree, unHash orig]
  extractHashKeys (File blob lastMod prev) = [unHash blob, unHash lastMod, unHash prev]
  extractHashKeys (Dir children) = unHash . snd <$> Map.toList children
  extractHashKeys NullCommit = []
  extractHashKeys (Commit _ changes parents) =
    let unChangeHash (Add h) = [unHash h]
        unChangeHash  Del    = []
     in (toList changes >>= (unChangeHash . _change)) ++ toList (fmap unHash parents)
  extractHashKeys (Blob _) = []


instance HFunctor M where
  hfmap f (Snapshot tree orig) = Snapshot (f tree) (f orig)
  hfmap f (File blob lastMod prev) = File (f blob) (f lastMod) (f prev)
  hfmap f (Dir children) = Dir (fmap f children)
  hfmap f NullCommit = NullCommit
  hfmap f (Commit msg changes parents) =
    let f' Change{..} = case _change of
          Add blob -> Change { _path = _path, _change = Add $ f blob}
          Del -> Change { _path = _path, _change = Del}
     in Commit msg (fmap f' changes) (fmap f parents)
  hfmap f (Blob x) = Blob x

instance HTraversable M where
  hmapM f (Snapshot tree orig) = do
    tree' <- f tree
    orig' <- f orig
    pure $ Snapshot tree' orig'
  hmapM f (File blob lastMod prev) = do
    blob' <- f blob
    lastMod' <- f lastMod
    prev' <- f prev
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
