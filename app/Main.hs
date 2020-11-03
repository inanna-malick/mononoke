{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

--------------------------------------------
import qualified Clay as Clay
import           Control.Concurrent.STM
import           Control.Monad (void)
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Data.List (intersperse)
import           Data.List.NonEmpty (toList, nonEmpty, (<|), NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import           Data.List.Split (splitOn)
import           Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Text.Lazy (unpack)
import           Data.Singletons.TH (SingI, sing)
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Ext.Flexbox
--------------------------------------------
import           HGit.Core.Types
import           HGit.Core.MergeTrie
import           HGit.GUI.CSS
import           HGit.GUI.Core
import           HGit.GUI.State
import           HGit.GUI.Messages
import           Merkle.Types.BlakeHash
import           Util.RecursionSchemes
import           Util.HRecursionSchemes as HR -- YOLO 420 SHINY AND CHROME
--------------------------------------------



type Minimizations = Set RawBlakeHash


branchBrowser
  :: Index UI
  -> Store UI
  -> Handler (SpawnPopup UI)
  -> BranchState UI
  -> Handler (FocusWIPT UI)
  -> Handler UpdateBranchState
  -> UI Element
branchBrowser commitSnapshotIndex store popRequest bs focusChangeHandler updateBranchStateHandler = do
    let extraBranches = (\(f,c) -> (OtherBranch f, c)) <$> bsBranches bs
    faUl #+ (fmap drawBranch $ [(MainBranch, bsMainBranch bs)] ++ extraBranches)

  where
    drawBranch (f, commit) = do
      esnap <- runExceptT $ updateSnapshotIndexLMMT store commitSnapshotIndex commit

      let commit' = faLi focusChangeHandler (unmodifiedWIP commit) [] (string "commit: ") UI.div
          snap'   = case esnap of
            Right snap -> faLi focusChangeHandler (unmodifiedWIP snap) [] (string "snap: ") UI.div
            Left e ->     string $ "unable to construct snapshot: " ++ show e

      let extraTags = if f == bsFocus bs then ["focus", "branch"] else ["branch"]
          forkAction focus = do
            liftIO $ popRequest $ SpawnRequestText "branch name" $ \newBranchName ->
              liftIO $ updateBranchStateHandler $ ForkFrom focus newBranchName
      let extraActions = if f == bsFocus bs
            then [("fa-code-branch", forkAction f)]
            else [("fa-code-branch", forkAction f), ("fa-search", liftIO $ updateBranchStateHandler $ ChangeFocus f)]

      case f of
        MainBranch    -> do
          faLiSimple extraTags "fa-code-branch" extraActions (string "main branch") $ faUl #+ [commit', snap']
        OtherBranch branchName -> do
          let delAction = liftIO $ do
                putStrLn $ "delete branch w/ name " ++ branchName
                updateBranchStateHandler $ DelBranch branchName
          faLiSimple extraTags "fa-code-branch" ([("fa-trash-alt", delAction)] ++ extraActions)
                                         (string branchName) $ faUl #+ [commit', snap']


-- can handle completing popup (eg it requests text)
drawModal :: String -> [UI Element] -> UI Element
drawModal hdr content = do
  root <- UI.div # withClass ["modal"]
  closeButton <- UI.button # set text "X"
  on UI.click closeButton $ \() -> delete root

  let titleBar = UI.div # withClass ["aesthetic-windows-95-modal-title-bar"]
                       #+ [ UI.div # withClass ["aesthetic-windows-95-modal-title-bar-text"]
                                   # set text hdr
                           , UI.div # withClass ["aesthetic-windows-95-modal-title-bar-controls"]
                                   #+ [UI.div # withClass ["aesthetic-windows-95-button-title-bar"]
                                             #+ [ element closeButton ]
                                       ]
                           ]

      w = UI.div # withClass ["aesthetic-windows-95-modal", "modal-popup"]
                #+ [ titleBar
                   , UI.div # withClass ["aesthetic-windows-95-modal-content", "popup-content"]
                           #+ content
                   ]
  element root #+ [UI.div # withClass ["modal-content"] #+ [w]]

      -- <div>
      --   <input class="aesthetic-windows-95-text-input" type="text" value="c:\aesthetic\src" />
      -- </div>
      -- <div class="margin-top">
      --   <textarea class="aesthetic-windows-95-text-input"></textarea>
      -- </div>



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



browseMergeTrie
  :: Handler (UpdateMergeTrie UI)
  -> Handler (FocusWIPT UI)
  -> TVar Minimizations
  -> InProgressCommit UI `Either` LMMT UI 'CommitT
  -> Fix (MergeTrie UI)
  -> UI Element
browseMergeTrie modifyMergeTrieHandler focusHandler _minimizations ipcOrC root = do
    (para f root) []
  where
    f :: MergeTrie UI (Fix (MergeTrie UI), [Path] -> UI Element) -> [Path] -> UI Element
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

    renderFiles path fs = fmap (renderFile path) fs

    -- TODO: if multiple files (merge conflict), could have button to accept one as cannonical
    renderFile path (_, (_ft, blob, _lastMod, _prevs)) =
      let extraButtons = case nonEmpty path of
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

    renderChild :: [Path] -> (Path, WIPT UI 'FileTree `Either` (Fix (MergeTrie UI), [Path] -> UI Element)) -> UI Element
    renderChild path (pathSegment, Left wipt) = do
      let pathNEL = NEL.reverse $ pathSegment :| reverse path -- append to path while preserving NEL by construction
      faLi focusHandler wipt [("fa-trash-alt", delChange pathNEL)]
                              (UI.code # set text pathSegment # set UI.class_ "path-segment")
                              (renderWIPTFileTree path pathSegment wipt)


    renderChild path (pathSegment, Right (mt, next)) = do
      let toFocusOn = case ipcOrC of
            Left ipc -> (["wip"], resolveMergeTrie (asWIPTCommit ipc) mt)
            Right c  -> (["persisted"], resolveMergeTrie (unmodifiedWIP c) mt)
      -- FIXME[err]: this one should result in a UI element denoting an MT error state
      let (extraTags, toFocusOn') = fmap (either undefined id) toFocusOn
          pathNEL = NEL.reverse $ pathSegment :| reverse path -- append to path while preserving NEL by construction
      x <- UI.div # withClass (extraTags ++ [typeTagName $ sing @'FileTree])
                 #+ [next (path ++ [pathSegment])]
      faLi focusHandler toFocusOn' [("fa-trash-alt", delChange pathNEL)]
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
                let pathNEL = NEL.reverse $ p :| reverse (path ++ [pathSegment]) -- append to path while preserving NEL by construction
                 in faLi focusHandler c [("fa-trash-alt", delChange pathNEL)]
                                        (UI.code # set text p # set UI.class_ "path-segment")
                                        (renderWIPTFileTree (path ++ [pathSegment]) p c)

          cs'
        File blob commit prev -> do
          faUl #+ ( [ faLi focusHandler commit [] (string "src commit") UI.div
                    ] ++
                     (fmap (\x -> faLi focusHandler x [] (string "prev iteration") UI.div) prev
                     ) ++
                    [ renderWIPTBlob focusHandler [] blob
                    ]
                  )
      element wrapper #+ [element x]

faUl :: UI Element
faUl = UI.ul # withClass ["fa-ul"]

withClass :: [String] -> UI Element -> UI Element
withClass = set UI.class_ . unwords


faLiSimple'
  :: [String] -- li extra class
  -> String
  -> UI Element -- tag
  -> UI Element -- sub-elems (content holder)
  -> UI Element
faLiSimple' liExtraClass faTag tag content = do
  icon' <- UI.span # withClass (["fa-li"] ++ liExtraClass)
                  #+ [ UI.italics # withClass ["fas", faTag]
                     ]

  hdr <- UI.span # withClass [] #+ [element icon', tag]

  UI.li # withClass (["fa-li-wrapper"]) #+ [element hdr, content]


faLiSimple
  :: [String] -- li extra class
  -> String
  -> [(String, UI ())] -- onHover actions
  -> UI Element -- tag
  -> UI Element -- sub-elems (content holder)
  -> UI Element
faLiSimple liExtraClass faTag onHoverButtons tag content = do
  let mkButton (fa, a) = do
        b <- UI.button #+ [UI.italics # withClass ["fas", fa]]
        on UI.click b $ \() -> a
        pure b

      dropdown = case onHoverButtons of
        [] -> []
        _  -> [UI.div # withClass ["dropdown"] #+ (fmap mkButton onHoverButtons)]

  icon' <- UI.span # withClass (["fa-li", "clickable-bullet"] ++ liExtraClass)
                  #+ ([ UI.italics # withClass ["fas", faTag]
                     ] ++ dropdown)

  hdr <- UI.span # withClass [] #+ [element icon', tag]

  UI.li # withClass (["fa-li-wrapper"]) #+ [element hdr, content]


faLi'
  :: forall (i :: MTag)
   . SingI i
  => Maybe (UI ()) -- focus action
  -> [(String, UI ())] -- onHover actions
  -> UI Element -- tag
  -> UI Element -- sub-elems (content holder)
  -> UI Element
faLi' mFocusAction onHoverButtons = faLiSimple [typeTagName $ sing @i] (typeTagFAIcon (sing @i)) buttons
  where
    mfocus = maybe [] (\focusAction -> [("fa-search", focusAction)]) mFocusAction
    buttons =  mfocus ++ onHoverButtons

faLi
  :: forall (i :: MTag)
   . SingI i
  => Handler (FocusWIPT UI)
  -> WIPT UI i
  -> [(String, UI ())] -- onHover
  -> UI Element
  -> UI Element
  -> UI Element
faLi focusHandler wipt = faLi' @i (Just action)
    where
      action = liftIO $ focusHandler $ wrapFocus (sing @i) wipt


drawCommitEditor
  :: BranchState UI
  -> Handler (SpawnPopup UI)
  -> Handler (UpdateMergeTrie UI)
  -> Handler (FocusWIPT UI)
  -> Maybe (InProgressCommit UI)
  -> UI Element
drawCommitEditor bs popRequest modifyMergeTrieHandler focusHandler ipc = do

  viewCommit <- case ipc of
    (Just InProgressCommit{..}) -> UI.div #+ [ string "changes:"
                                             , viewChanges ipcChanges
                                             , UI.br
                                             , string "parents:"
                                             , viewParents ipcParentCommits
                                             , viewMessage ipcMsg
                                             , UI.br
                                             , finalizeCommit
                                             , UI.br
                                             , resetCommit
                                             ]
    Nothing -> string "no in progress commit"

  UI.div #+ [ addChanges
            , element viewCommit
            , UI.br
            , string "add parent to commit: "
            , addParent
            , string "branches:"
            ]

  where
    addParent   = do
      let branches = [("main", bsMainBranch bs)] ++ bsBranches bs
          options = fmap mkOption branches
          mkOption (s, c) = do
            opt <- UI.option # set UI.value s # set text s
            on UI.click opt $ \() -> void $ do
              liftIO $ modifyMergeTrieHandler (AddParent c)
            pure opt
      UI.select #+ options

    finalizeCommit = do
      finalize <- UI.button # set text "finalize commit [!]"
      on UI.click finalize $ \() -> void $ do
        liftIO $ modifyMergeTrieHandler Finalize
      pure finalize

    resetCommit = do
      reset <- UI.button # set text "reset commit [!]"
      on UI.click reset $ \() -> void $ do
        liftIO $ modifyMergeTrieHandler Reset
      pure reset

    viewParents ps = do
      elems <- NEL.toList <$> traverse viewParent ps
      faUl #+ fmap element elems

    viewParent wipt = faLi focusHandler wipt [] (string "parent commit") UI.div

    viewMessage msg = string ("msg: \"" ++ msg ++  "\"")


    renderChange :: NonEmpty Path -> ChangeType (WIPT UI) -> UI Element
    renderChange p (Add wipt) =
      faLiSimple ["add"] "fa-plus-circle" (removeChange p) (string $ "Add: " ++ renderPath p) $ faUl #+ [renderWIPTBlob focusHandler [] wipt]
    renderChange p Del =
      faLiSimple ["del"] "fa-minus-circle" (removeChange p) (string $ "Del: " ++ renderPath p) UI.div

    removeChange nel = [("fa-trash-alt", liftIO $ modifyMergeTrieHandler $ RemoveChange nel)]



    viewChanges cs = do
      faUl #+ (uncurry renderChange <$> (Map.toList cs))

    renderPath = mconcat . intersperse "/" . toList


    addChanges = do
      addButton <- UI.button # set text "add"
      delButton <- UI.button # set text "del"

      pathInput     <- UI.input

      on UI.click delButton $ \() -> void $ runMaybeT $ do
        path <- lift (UI.get UI.value pathInput) >>= MaybeT . pure . parsePath
        liftIO $ modifyMergeTrieHandler (ApplyChange path Del)

      on UI.click addButton $ \() -> void $ runMaybeT $ do
        path <- lift (UI.get UI.value pathInput) >>= MaybeT . pure . parsePath
        liftIO $ popRequest $ SpawnRequestText "file contents" $ \s -> liftIO $ do
          modifyMergeTrieHandler (ApplyChange path $ Add $ modifiedWIP $ Blob s)

      UI.div #+ [ string "path:"
                , element pathInput
                , UI.br
                , element delButton
                , element addButton
                ]


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


focusButton
  :: forall (x:: MTag)
   . SingI x
  => Handler (FocusWIPT UI)
  -> WIPT UI x
  -> UI Element
focusButton focusHandler wipt = do
    focus <- UI.button # set UI.class_ (typeTagName' wipt ++ "-focus")
                       #+ [UI.italics # set UI.class_ ("fas " ++ typeTagFAIcon' wipt)]
    on UI.click focus $ \() -> do
      liftIO $ focusHandler $ wrapFocus sing wipt
    pure focus



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

    browseMononoke' (File b l p) = (faUl #+) $
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



nestedUL :: UI Element
nestedUL = UI.ul # set (attr "style") "margin-left:1em"


nestedDiv :: UI Element
nestedDiv = UI.div # set (attr "style") "margin-left:1em"



main :: IO ()
main = do
  startGUI (defaultConfig { jsStatic = Just "static"}) setup


-- NOTE/TODO: this could all be in a single STM transaction. wild.
updateSnapshotIndexLMMT
  :: MonadIO m
  => Store m
  -> Index m
  -> LMMT m 'CommitT
  -> ExceptT (NonEmpty MergeError) m (LMMT m 'SnapshotT)
updateSnapshotIndexLMMT store index commit = do
  msnap <- lift $ (iRead index) (hashOfLMMT commit)
  (HC (Tagged _ commit')) <- lift $ fetchLMMT commit
  case msnap of
    Just h  -> do
      pure $ expandHash (sRead store) h
    Nothing -> do
      snap <- makeSnapshot (hfmap unmodifiedWIP commit') (iRead index) (sRead store)
      let wipt = modifiedWIP snap
      uploadedSnap <- lift $ uploadWIPT (sWrite store) wipt
      lift $ (iWrite index) (hashOfLMMT commit) (hashOfLMMT uploadedSnap)
      pure uploadedSnap


updateSnapshotIndexWIPT
  :: MonadIO m
  => StoreRead m
  -> IndexRead m
  -> WIPT m 'CommitT
  -> ExceptT (NonEmpty MergeError) m (M (WIPT m) 'SnapshotT)
updateSnapshotIndexWIPT store index commit = do
  (HC (Tagged _ commit')) <- lift $ fetchWIPT commit
  makeSnapshot commit' index store


setup :: Window -> UI ()
setup root = void $ do
  void $ getHead root #+ [ mkElement "style" # set (UI.text) (unpack (Clay.render css))
                         ]
  UI.addStyleSheet root "all.css" -- fontawesome
  UI.addStyleSheet root "aesthetic.css" -- vaporwave! https://github.com/torch2424/aesthetic-css/blob/master/aesthetic.css


  commitSnapshotIndexTVar <- liftIO . atomically $ newTVar Map.empty
  let commitSnapshotIndex = stmIOIndex commitSnapshotIndexTVar

  inProgressCommitTVar :: TVar (Maybe (InProgressCommit UI))
    <- liftIO . atomically $ newTVar $ Nothing

  blobStoreTvar <- liftIO . atomically $ newTVar emptyBlobStore
  let blobStore = stmIOStore blobStoreTvar


  initCommitHash <- uploadM (sWrite blobStore) $ commit3
  let innitCommit = expandHash (sRead blobStore) initCommitHash

  let initialBranchState = BranchState
                         { bsMainBranch = innitCommit
                         , bsBranches = []
                         , bsFocus = MainBranch
                         }


  branchState <- liftIO . atomically $ newTVar initialBranchState
  minimizations <- liftIO . atomically $ newTVar (Set.empty)

  let updateCurrentBranch :: LMMT UI 'CommitT -> STM ()
      updateCurrentBranch commit = do
        bs <- readTVar branchState
        let focus = bsFocus bs
        case focus of
          MainBranch -> do
            writeTVar branchState $ bs { bsMainBranch = commit}
          OtherBranch b -> do
            let update (s,c) | s == b    = (s, commit) -- update commit if exists (janky? maybe, idk)
                             | otherwise = (s,c)
            writeTVar branchState $ bs { bsBranches = update <$> bsBranches bs}

  (updateBranchStateEvent, updateBranchStateHandler) <- liftIO $ newEvent
  (focusChangeEvent, focusChangeHandler) <- liftIO $ newEvent
  (modifyMergeTrieEvent, modifyMergeTrieHandler) <- liftIO $ newEvent
  (popupEvent, popupHandler) <- liftIO $ newEvent


  -- empty root nodes for various UI elements
  mergeTrieRoot <- infraDiv
  sidebarRoot <- infraDiv # withClass ["sidebar"]
  modalRoot <- UI.div -- not infra because often invisible, infra controls display



  let handleSpawnPopup (SpawnError s) = void $ do
        _ <- element modalRoot # set children []
        element modalRoot #+ [ drawModal "error!" [string s] ]

      handleSpawnPopup (SpawnRequestText s act) = void $ do
        _ <- element modalRoot # set children []
        inputElem <- UI.input # withClass ["aesthetic-windows-95-text-input"]
        on UI.sendValue inputElem $ \input -> do
          act input
          element modalRoot # set children []
        element modalRoot #+ [drawModal s [ string "req: "
                                          , element inputElem
                                          ]
                             ]


  let popError :: Show e => e -> UI ()
      popError e = liftIO $ popupHandler $ SpawnError (show e)

  let extractFT :: M x 'SnapshotT -> x 'FileTree
      extractFT (Snapshot ft _ _) = ft


  -- errors on key not found - not sure how to handle this, need
  -- generic 'shit broke' error channel, eg popup/alert
  -- TODO: need to run everything in exceptT so I can pop an error when shit like this happens
  let getCurrentBranch :: STM (BranchFocus, LMMT UI 'CommitT)
      getCurrentBranch = do
        bs <- readTVar branchState
        let focus = bsFocus bs
        case focus of
          MainBranch -> pure $ (focus, bsMainBranch bs)
          OtherBranch b -> maybe undefined (pure . (focus,)) $ lookup b (bsBranches bs)

  let redrawSidebar bs mipc = void $ do
        _ <- element sidebarRoot # set children []
        element sidebarRoot #+ [ drawCommitEditor bs popupHandler modifyMergeTrieHandler focusChangeHandler mipc
                               , branchBrowser commitSnapshotIndex blobStore popupHandler bs focusChangeHandler updateBranchStateHandler
                               ]


  let redrawMergeTrie mt = void $ do
        ipcOrC <- liftIO $ atomically $ do
          mipc <- readTVar inProgressCommitTVar
          case mipc of
            Just ipc -> pure $ Left ipc
            Nothing ->  do
              Right . snd <$> getCurrentBranch

        _ <- element mergeTrieRoot # set children []
        element mergeTrieRoot #+ [browseMergeTrie modifyMergeTrieHandler focusChangeHandler minimizations ipcOrC mt]

  let handleErr m = do
        x <- runExceptT m
        case x of
          Right () -> pure ()
          Left e   -> popError e

      handleMMTE :: UpdateMergeTrie UI -> UI ()
      handleMMTE msg = handleErr $ do
        liftIO $ putStrLn $ "handle MMTE msg"
        liftIO $ putStrLn $ show msg
        mcommit <- handleMMTE' msg

        mt <- case mcommit of
          Nothing -> do
            -- redraw commit editor
            bs <- liftIO $ atomically $ readTVar branchState
            lift $ redrawSidebar bs Nothing

            commit <-  liftIO $ atomically $ snd <$> getCurrentBranch
            snap' <- updateSnapshotIndexLMMT blobStore commitSnapshotIndex commit
            (HC (Tagged _ snap)) <- lift $ fetchLMMT snap'
            let ft = unmodifiedWIP $ extractFT snap
            lift $ buildMergeTrie emptyMergeTrie ft

          Just ipc@InProgressCommit{..} -> do
            -- redraw commit editor
            bs <- liftIO $ atomically $ readTVar branchState
            lift $ redrawSidebar bs $ Just ipc

            let changes = uncurry Change <$> Map.toList ipcChanges
                commit = Commit ipcMsg changes ipcParentCommits

            fmap snd $ makeMT commit (iRead commitSnapshotIndex) (sRead blobStore)

        lift $ redrawMergeTrie mt
        pure ()

      handleMMTE' :: UpdateMergeTrie UI -> ExceptT (NonEmpty MergeError) UI (Maybe (InProgressCommit UI))
      handleMMTE' (RemoveChange path) = liftIO $ atomically $ do
          c <- readTVar inProgressCommitTVar
          nextCommit <- case c of
                (Just ipc) -> pure $ Just $ ipc { ipcChanges = Map.delete path (ipcChanges ipc) }
                Nothing -> pure Nothing
          writeTVar inProgressCommitTVar nextCommit
          pure nextCommit

      handleMMTE' (ApplyChange path ct) = liftIO $ atomically $ do
          c <- readTVar inProgressCommitTVar
          nextCommit <- case c of
                (Just ipc) -> pure $ Just $ ipc { ipcChanges = Map.insert path ct (ipcChanges ipc) }
                Nothing ->    do
                  commit <- snd <$> getCurrentBranch
                  pure $ Just $ InProgressCommit { ipcChanges = Map.singleton path ct
                                          , ipcParentCommits = unmodifiedWIP commit :| []
                                          , ipcMsg = "todo"
                                          }
          writeTVar inProgressCommitTVar nextCommit
          pure nextCommit

      handleMMTE' (AddParent parentToAdd) = liftIO $ atomically $ do
          c <- readTVar inProgressCommitTVar
          nextCommit <- case c of
                (Just ipc) -> pure $ Just $ ipc { ipcParentCommits = unmodifiedWIP parentToAdd <| ipcParentCommits ipc }
                Nothing -> do
                  commit <- snd <$> getCurrentBranch
                  pure $ Just $ InProgressCommit { ipcChanges = Map.empty
                                                 , ipcParentCommits = fmap unmodifiedWIP $ parentToAdd :| [commit]
                                                 , ipcMsg = "todo"
                                                 }
          writeTVar inProgressCommitTVar nextCommit
          pure nextCommit

      handleMMTE' Reset = liftIO $ atomically $ do
          let nextCommit = Nothing
          writeTVar inProgressCommitTVar nextCommit
          pure nextCommit

      handleMMTE' Finalize = do
          -- FIXME: interleaving STM and IO actions here, b/c need to upload commit. janky?
          mipc <- liftIO $ atomically $ do
            readTVar inProgressCommitTVar
          case mipc of
            Nothing  -> pure Nothing -- no-op, nothing to finalize
            Just InProgressCommit{..} -> do
              let changes = uncurry Change <$> Map.toList ipcChanges
                  commit = modifiedWIP $ Commit ipcMsg changes ipcParentCommits

              uploadedCommit <- lift $ uploadWIPT (sWrite blobStore) commit


              -- update snapshot index. will error out if this commit is invalid, preventing it from causing tvar updates
              _ <- updateSnapshotIndexLMMT blobStore commitSnapshotIndex uploadedCommit

              let mipc' = Nothing

              bs <- liftIO $ atomically $ do
                -- assertion: IPC and current branch are always intertwined, so this is safe
                updateCurrentBranch uploadedCommit
                writeTVar inProgressCommitTVar mipc'
                bs <- readTVar branchState
                pure bs

              lift $ redrawSidebar bs mipc'

              pure Nothing



  -- discarded return value deregisters handler
  _ <- onEvent popupEvent handleSpawnPopup

  -- discarded return value deregisters handler
  _ <- onEvent modifyMergeTrieEvent handleMMTE

  -- TODO: only reset on DelBranch if focus changed (current branch deleted == auto-reset)
  liftIO $ modifyMergeTrieHandler $ Reset

  browserRoot <- faUl
  -- discarded return value deregisters handler
  _ <- onEvent focusChangeEvent $ \focus -> do
    void $ element browserRoot # set children []
    void $ element browserRoot #+ [browseWIPT focusChangeHandler minimizations focus]
    pure ()


  liftIO $ focusChangeHandler $ wrapFocus sing $ unmodifiedWIP innitCommit

  _ <- onEvent updateBranchStateEvent $ \ubs -> do
    bs' <- case ubs of
      ForkFrom f s -> do
        liftIO $ putStrLn "fork from"
        liftIO $ atomically $ do
          bs <- readTVar branchState
          commit <- case f of
            MainBranch -> pure $ bsMainBranch bs
            OtherBranch b -> pure $ maybe (error "todo") id $ lookup b (bsBranches bs)

          -- TODO: spawn popup requesting branch name, prepop'd with name of branch being forked from

          let bs' = bs { bsBranches = bsBranches bs ++ [(s, commit)] }
          writeTVar branchState bs'
          pure bs'
      DelBranch s -> do
        liftIO $ putStrLn "del branch: todo"
        liftIO $ atomically $ do
          bs <- readTVar branchState

          let bs' = bs
                  { bsBranches = filter ((/= s) . fst) $ bsBranches bs
                  , bsFocus = if bsFocus bs == OtherBranch s then MainBranch else bsFocus bs
                  }

          writeTVar branchState bs'
          pure bs'
      ChangeFocus f -> do
        liftIO $ putStrLn "changefocus"
        liftIO $ atomically $ do
          -- TODO: check in progress commit, if set -> error
          bs <- readTVar branchState
          -- TODO: error if focus not found in list of branches
          let bs' = bs { bsFocus = f }
          writeTVar branchState bs'
          pure bs'

    -- dispatch reset after change focus - will redraw merge trie & reset + redraw commit editor
    case ubs of
      _ -> -- FIXME: just run in all cases b/c that'll trigger redraw of commit editor - could optimize more here
        liftIO $ modifyMergeTrieHandler Reset

    mipc <- liftIO $ atomically $ readTVar inProgressCommitTVar
    redrawSidebar bs' mipc
    pure ()

  bs <- liftIO $ atomically $ readTVar branchState

  redrawSidebar bs Nothing

  void $ flex_p (getBody root)
                [ (element sidebarRoot, flexGrow 1)
                , (element mergeTrieRoot, flexGrow 2)
                , ( infraDiv #+ [element browserRoot]
                  , flexGrow 2
                  )
                ]
  void $ getBody root #+ [element modalRoot]


infraDiv :: UI Element
infraDiv = UI.div # withClass ["infra"]
