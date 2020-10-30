{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module HGit.Core.Types.Render where


--------------------------------------------
import           Data.Functor.Const (Const(..))
import           Data.Functor.Compose
import           Data.List (intersperse)
import           Data.List.NonEmpty (toList)
import qualified Data.Map.Strict as Map
import           Data.Singletons.TH
--------------------------------------------
import           HGit.Render.Utils
import           HGit.Core.Types
import           Util.HRecursionSchemes as HR -- YOLO 420 SHINY AND CHROME
--------------------------------------------


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
renderM (Commit msg changes parents) =
     Const $ mconcat [["Commit \"" ++ msg ++ "\""]] ++
  ( indent $
           [ ["changes:"] ++ (indent $ (renderChange <$> changes))
           , ["parents:"] ++ (indent $ (getConst <$> toList parents))
           ]
  )
renderM (Blob x) = Const ["Blob: " ++ x]


renderChange :: Change (Const [String]) -> [String]
renderChange Change{..} = case _change of
    Del   -> [renderPath _path ++ ": Del"]
    Add x -> [renderPath _path ++ ": Add: "] ++ getConst x
  where
    renderPath = mconcat . intersperse "/" . toList

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

