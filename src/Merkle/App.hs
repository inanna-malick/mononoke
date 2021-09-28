{-# LANGUAGE TypeFamilies #-}



module Merkle.App where


import           Data.List.NonEmpty
import Merkle.Bonsai.Types
import Merkle.Generic.HRecursionSchemes





diffLocalState :: Path -> LMMT m 'FileTree -> m [Change (Term M)]
diffLocalState base snapshot = processRoot snapshot
  where
    processRoot :: LMMT m 'FileTree -> m [Change (Term M)]
    processRoot = do
      undefined
    process :: NonEmpty Path -> LMMT m 'FileTree -> m [Change (Term M)]
    process = undefined -- TODO: unwrap lmmt. bleh
    -- process base (Term (Dir subdirs)) = do
    --   undefined
    -- process base (Term (File file)) = do
    --   undefined

