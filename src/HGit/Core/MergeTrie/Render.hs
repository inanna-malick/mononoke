{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module HGit.Core.MergeTrie.Render where

--------------------------------------------
import           Data.Functor.Foldable (cata)
import           Data.Fix (Fix(..))
import qualified Data.Map.Strict as Map
--------------------------------------------
import           HGit.Core.Types
import           HGit.Core.Types.Render (renderWIPT)
import           HGit.Core.MergeTrie
import           HGit.Render.Utils
--------------------------------------------



renderMergeTrie :: Fix (MergeTrie m) -> [String]
renderMergeTrie = cata f
  where
    f :: MergeTrie m [String] -> [String]
    f mt =
      let g :: (Path, ((WIPT m 'FileTree) `Either` [String])) -> [String]
          g (k, (Left wipt)) = [k ++ ": "] ++ renderWIPT wipt
          g (k, (Right v)) = [k ++ ": "] ++ v
          children :: [[String]]
          children = if Map.null (mtChildren mt)
            then []
            -- TODO handle either
            else [["children"] ++ (indent $ fmap g $ Map.toList $ mtChildren mt)]
          change :: [[String]]
          change = case (mtChange mt) of
            Nothing -> []
            Just (Add _) -> [["add"]]
            Just Del -> [["del"]]
          files :: [[String]]
          files = if null (mtFilesAtPath mt)
            then []
            else [["files:"] ++ (indent $ pure . showHash <$> Map.keys (mtFilesAtPath mt))]
       in "MergeTrie:" : indent
          ( files ++ change ++ children
          )

