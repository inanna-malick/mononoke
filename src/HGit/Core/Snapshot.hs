{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module HGit.Core.Snapshot where

--------------------------------------------
import           Data.Aeson as AE
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import           Data.Functor.Compose
import           Data.List.NonEmpty (NonEmpty, toList)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import           Data.Text.Encoding (decodeLatin1, encodeUtf8)
import           Data.Singletons.TH
import           GHC.Generics
--------------------------------------------
import           HGit.Core.Types
import           Merkle.Higher.Types
import           Merkle.Higher.Store
import           Merkle.Higher.Store.Deref
import           Util.RecursionSchemes as R
import           Util.HRecursionSchemes as HR -- YOLO 420 SHINY AND CHROME
--------------------------------------------

-- type Index m = Hash 'CommitT -> m (Maybe (Hash 'SnapshotT))
-- type ReadStore m = forall x. Hash x -> m (M Hash x) -- fails in m (hash lookup err is fatal)
-- type WriteStore m = forall x. M Hash x -> m (Hash x)

-- -- TODO: return type that encapsulates new structure/old distinction?
-- -- TODO: problem is, not updating old structure, creating new snapshot w/ no link to prev snapshot
-- -- TODO: why not though? could have snapshots mimic commit link structure - not sure I like doing so tho

-- -- update merge-in-progress data structure
-- advanceState
--   :: Term (LMM m) 'CommitT
--   -> Index m
--   -> ReadStore m
--   -> WriteStore m
--   -> m (NonEmpty (Hash 'SnapshotT))
-- advanceState i r w = undefined
