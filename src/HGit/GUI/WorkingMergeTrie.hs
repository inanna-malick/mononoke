{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module HGit.GUI.WorkingMergeTrie where

--------------------------------------------
import           Control.Concurrent.STM
import           Data.Functor.Compose
import           Data.Functor.Const
import           Data.Functor.Foldable (Fix(..), cata)
import           Data.List.NonEmpty (NonEmpty, nonEmpty)
import           Data.Singletons.TH (sing)
import qualified Data.Map as Map
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
--------------------------------------------
import           HGit.Core.MergeTrie
import qualified HGit.Core.Types.Render
import           HGit.Core.Types
import           HGit.GUI.BrowseWIPT
import           HGit.GUI.Core
import           HGit.GUI.Elements
import           HGit.GUI.State
import           HGit.Generic.HRecursionSchemes
--------------------------------------------


data UpdateMergeTrie m
  = ApplyChange (NonEmpty Path) (ChangeType (WIPT m))
  | RemoveChange (NonEmpty Path)
  | AddParent (LMMT m 'CommitT)
  | Reset
  | Finalize String -- finalize commit w/ message

instance Show (UpdateMergeTrie m) where
  show (ApplyChange p c) =
    let ctmapShim f (Add a) = Add (f a)
        ctmapShim _ Del = Del
        cmapShim f Change{..} = Change _path $ ctmapShim f _change
     in mconcat [ "ApplyChange: "
                , unlines $ HGit.Core.Types.Render.renderChange
                          $ cmapShim (Const . HGit.Core.Types.Render.renderWIPT)
                          $ Change p c
                ]
  show (RemoveChange p) = "RemoveChange: " ++ show p
  show (AddParent _) = "AddParent: todo"
  show Reset = "Reset"
  show (Finalize msg) = "Finalize, msg: " ++ msg


browseMergeTrie
  :: Handler (UpdateMergeTrie UI)
  -> Handler (FocusWIPT UI)
  -> TVar Minimizations
  -> InProgressCommit UI `Either` LMMT UI 'CommitT
  -> Fix (ErrorAnnotatedMergeTrie UI) `Either` Fix (MergeTrie UI)
  -> UI Element
browseMergeTrie modifyMergeTrieHandler focusHandler _minimizations ipcOrC eroot -- TODO: revisit 'ipc or c'
  = case eroot of
      Left  e -> cata g e []
      Right x -> cata f x []

  where

    g :: ErrorAnnotatedMergeTrie UI ([Path] -> UI Element) -> [Path] -> UI Element
    g (Compose (Right (Compose (me, mt)))) path = do
      faUl #+ mconcat
            [ case me of
                Nothing -> []
                Just e  -> [string $ "error at this node: " ++ show e, UI.br]
            , renderChildren path $ mtChildren mt
            , renderFiles path $ Map.toList $ mtFilesAtPath mt
            , maybe [] (pure . renderChange path) $ mtChange mt
            ]
    g (Compose (Left fmt)) path = cata f fmt path


    f :: MergeTrie UI ([Path] -> UI Element) -> [Path] -> UI Element
    f mt path = do
      faUl #+ mconcat
            [ renderChildren path $ mtChildren mt
            , renderFiles path $ Map.toList $ mtFilesAtPath mt
            , maybe [] (pure . renderChange path) $ mtChange mt
            ]


    delChange :: NonEmpty Path -> UI ()
    delChange path = do
      liftIO $ putStrLn $ "send del to:"
      liftIO $ print path
      liftIO $ modifyMergeTrieHandler $ ApplyChange path Del

    renderFiles path fs =
      let resolveMergeConflict nel b = liftIO $ modifyMergeTrieHandler (ApplyChange nel $ Add b)
          -- button to accept one file in a merge conflict as the canonical version
          mkExtraButtons b = if length fs >= 2
            then case nonEmpty path of
              -- TODO: more appropriate button icon, mb
              Just nel -> [("fa-code", resolveMergeConflict nel b)]
              Nothing -> [] -- files at '/' root path are invalid anyway
            else []

       in fmap (renderFile mkExtraButtons path) fs

    renderFile mkExtraButtons path (_, (_ft, SnapshotFile blob _lastMod _prevs)) =
      let extraButtons = mkExtraButtons blob ++ case nonEmpty path of
            Nothing -> [] -- a file at the root path is an error anyway..
            Just nel -> [("fa-trash-alt", delChange nel)]

       in faLiSimple' [] "fa-chevron-right" (string "file candidate")
                                            (faUl #+ [renderWIPTBlob focusHandler extraButtons blob])

    renderChange :: [Path] -> ChangeType (WIPT UI) -> UI Element
    renderChange p (Add wipt) =
      faLiSimple ["add"] "fa-plus-circle" (removeChange p) (string "Add") $ faUl #+ [renderWIPTBlob focusHandler [] wipt]
    renderChange p Del =
      faLiSimple ["del"] "fa-minus-circle" (removeChange p) (string "Del") UI.div

    removeChange p = case nonEmpty p of
      Nothing -> []
      Just nel -> [("fa-trash-alt", liftIO $ modifyMergeTrieHandler $ RemoveChange nel)]

    renderChildren path c = fmap (renderChild path) (Map.toList c)

    renderChild :: [Path] -> (Path, WIPT UI 'FileTree `Either` ([Path] -> UI Element)) -> UI Element
    renderChild path (pathSegment, Left wipt) = do
      let pathNEL = appendNEL path pathSegment
      faLi focusHandler wipt [("fa-trash-alt", delChange pathNEL)]
                              (UI.code # set text pathSegment # set UI.class_ "path-segment")
                              (renderWIPTFileTree path pathSegment wipt)


    renderChild path (pathSegment, Right next) = do
      let extraTags = case ipcOrC of
            Left _  -> ["wip"]
            Right _ -> ["persisted"]
      let pathNEL = appendNEL path pathSegment
      x <- UI.div # withClass (extraTags ++ [typeTagName $ sing @'FileTree])
                 #+ [next (path ++ [pathSegment])]
      -- TODO: attempt to resolve, show resulting node if possible, if empty 'path deleted', if error idk but something for that case too
      -- TODO: will require para instead of cata, I think
      faLiSimple [] "fa-folder-open" [("fa-trash-alt", delChange pathNEL)]
                                     (string pathSegment # set UI.class_ "path-segment")
                                     (element x)


    renderWIPTFileTree :: [Path] -> Path -> WIPT UI 'FileTree -> UI Element
    renderWIPTFileTree path pathSegment wipt = do
      (HC (Tagged _h ft)) <- fetchWIPT wipt
      let extraTags = case wipt of
            (Term (HC (L _))) -> ["persisted"]
            (Term (HC (R _))) -> ["wip"]
      wrapper <- UI.div # withClass ([typeTagName' ft] ++ extraTags)
      x <- case ft of
        Dir cs -> do
          let cs' = faUl #+ (renderDirEntry <$> Map.toList cs)
              renderDirEntry (p,c) =
                -- append to path while preserving NEL by construction
                let pathNEL = appendNEL path pathSegment
                 in faLi focusHandler c [("fa-trash-alt", delChange pathNEL)]
                                        (UI.code # set text p # set UI.class_ "path-segment")
                                        (renderWIPTFileTree (path ++ [pathSegment]) p c)

          cs'
        File (SnapshotFile blob commit prev) -> do
          faUl #+ ( [ faLi focusHandler commit [] (string "src commit") UI.div
                    ] ++
                     (fmap (\x -> faLi focusHandler x [] (string "prev iteration") UI.div) prev
                     ) ++
                    [ renderWIPTBlob focusHandler [] blob
                    ]
                  )
      element wrapper #+ [element x]

