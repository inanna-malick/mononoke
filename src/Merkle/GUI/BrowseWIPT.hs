{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Merkle.GUI.BrowseWIPT where

--------------------------------------------
import           Control.Concurrent.STM
import           Control.Monad (void)
import           Control.Monad.Trans
import           Data.List (intersperse)
import           Data.List.NonEmpty (toList, nonEmpty, NonEmpty(..))
import           Data.List.Split (splitOn)
import           Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Singletons.TH (SingI, sing)
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
--------------------------------------------
import           Merkle.Bonsai.Types
import           Merkle.GUI.Core
import           Merkle.GUI.Elements
import           Merkle.Generic.BlakeHash
import           Merkle.Generic.HRecursionSchemes
--------------------------------------------



type Minimizations = Set RawBlakeHash

renderWIPTBlob :: Handler (FocusWIPT UI) -> [(String, UI ())] -> WIPT UI 'BlobT -> UI Element
renderWIPTBlob focusHandler actions wipt = do
  let extraTags = case wipt of
        (Term (HC (L _))) -> ["persisted"]
        (Term (HC (R _))) -> ["wip"]

  (HC (Tagged _h blob)) <- fetchWIPT wipt
  case blob of
    Blob b -> do
      body <- UI.div # withClass (extraTags ++ [typeTagName $ sing @'BlobT])
                    #+ [string $ "\"" ++ b ++ "\""]
      faLi focusHandler wipt actions (string "file blob")
                                      (element body)




-- construct a NEL from a list and an element,
-- but with the element appended to the list instead of prepended
appendNEL :: [a] -> a -> NonEmpty a
appendNEL xs x = maybe (pure x) (<> pure x) $ nonEmpty xs


parsePath :: String -> Maybe (NonEmpty Path)
parsePath s = do
  case nonEmpty (filter (/= "") $ splitOn "/" s) of
    Nothing -> Nothing
    Just xs -> Just xs


browseWIPT
  :: Handler (FocusWIPT UI)
  -> TVar Minimizations
  -> FocusWIPT UI
  -> UI Element
browseWIPT focusHandler minimizations focus = case focus of
    SnapshotF root -> (getConst $ hpara (uiWIPAlg focusHandler minimizations) root)
    FileTreeF root -> (getConst $ hpara (uiWIPAlg focusHandler minimizations) root)
    CommitF   root -> (getConst $ hpara (uiWIPAlg focusHandler minimizations) root)
    BlobF     root -> (getConst $ hpara (uiWIPAlg focusHandler minimizations) root)


browseLMMT
  :: Handler (FocusWIPT UI)
  -> TVar Minimizations
  -> FocusLMMT UI
  -> UI Element
browseLMMT focusHandler minimizations focus = case focus of
    SnapshotF root -> (getConst $ hpara (uiLMMAlg focusHandler minimizations) root)
    FileTreeF root -> (getConst $ hpara (uiLMMAlg focusHandler minimizations) root)
    CommitF   root -> (getConst $ hpara (uiLMMAlg focusHandler minimizations) root)
    BlobF     root -> (getConst $ hpara (uiLMMAlg focusHandler minimizations) root)



uiWIPAlg
  :: Handler (FocusWIPT UI)
  -> TVar Minimizations
  -> RAlg (WIP UI) (Const (UI Element))
uiWIPAlg focusHandler minimizations (HC (L lmmt)) = Const $ do
      browseLMMT focusHandler minimizations (wrapFocus sing $ lmmt)
uiWIPAlg focusHandler minimizations wipt@(HC (R m)) = Const $ do
      let action = liftIO $ focusHandler $ wrapFocus sing $ Term $ hfmap _tag wipt
      getConst $ browseMononoke minimizations action ["wip"] $ hfmap _elem m


uiLMMAlg
  :: Handler (FocusWIPT UI)
  -> TVar Minimizations
  -> RAlg (LMM UI) (Const (UI Element))
uiLMMAlg focusHandler minimizations lmm = Const $ do
      m <-  fetchLMM lmm
      let action = liftIO $ focusHandler $ wrapFocus sing $ unmodifiedWIP $ Term $ hfmap _tag lmm
      getConst $ browseMononoke minimizations action ["persisted"] $ hfmap _elem m


browseMononoke
  :: forall (i :: MTag)
   . SingI i
  => TVar Minimizations
  -> UI ()
  -> [String]
  -> (Tagged Hash `HCompose` M) (Const (UI Element)) i
  -> (Const (UI Element)) i
browseMononoke minimizations focusAction extraTags (HC (Tagged h m)) = Const $ do
      content <- UI.div # withClass ([typeTagName' m] ++ extraTags) #+ [browseMononoke' m]
      faLi' @i (Just focusAction) [ ("fa-eye", toggleNode (getConst h) content)
                                  ]
                                  (string $ typeTagName' m) (pure content)

  where
    toggleNode :: RawBlakeHash -> Element -> UI ()
    toggleNode k node = void $ do
      isMinimized <- liftIO $ atomically $ do
        wasMinimized <- Set.member k <$> readTVar minimizations
        if wasMinimized
          then do
            modifyTVar minimizations (Set.delete k)
          else do
            modifyTVar minimizations (Set.insert k)
        pure $ not wasMinimized

      if isMinimized
        then element node # set UI.class_ "hidden"
        else element node # set UI.class_ ""

    renderChange :: Change (Const (UI Element)) -> UI Element
    renderChange Change{..} =
        let renderPath = mconcat . intersperse "/" . toList
        in case _change of
              Add (Const next) ->
                faLiSimple ["add"] "fa-plus-circle" [] (string $ "Add: " ++ renderPath _path) $ faUl #+ [next]
              Del -> faLiSimple ["del"] "fa-minus-circle" [] (string $ "Del: " ++ renderPath _path) UI.div



    -- returns fa-ul item or div/string/etc
    browseMononoke' :: forall (x:: MTag). M (Const (UI Element)) x -> UI Element
    browseMononoke' (Snapshot t o ps) = (faUl #+) $
      [ faLiSimple' [] "fa-chevron-right" (string "root") $ getConst t
      , faLiSimple' [] "fa-chevron-right" (string "generated from commit") $ getConst o
      ] ++
      ( faLiSimple' [] "fa-chevron-right" (string "parent snapshot") . getConst <$> ps)

    browseMononoke' (File (SnapshotFile b l p)) = (faUl #+) $
      [ faLiSimple' [] "fa-chevron-right" (string "blob") $ getConst b
      , faLiSimple' [] "fa-chevron-right" (string "last modified") $ getConst l
      ] ++ (faLiSimple' [] "fa-chevron-right" (string "previous incarnation") . getConst <$>  p)

    browseMononoke' (Dir cs) = (faUl #+) $
          let renderChild (k, Const v) = faLiSimple' [] "fa-chevron-right" (string k) v
           in renderChild <$> Map.toList cs

    browseMononoke' NullCommit = string "NullCommit"

    browseMononoke' (Commit msg cs ps) = (faUl #+) $
      [ faLiSimple' [] "fa-comment-alt" (string $ "msg: " ++ msg) UI.div ] ++
      (renderChange <$> cs) ++
      ( faLiSimple' [] "fa-chevron-right" (string "parent") . getConst <$> toList ps)

    browseMononoke' (Blob c) =  UI.string $ "\"" ++ c ++ "\""
