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
import           Data.List.Split (splitOn)
import           Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Functor.Compose
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
import           HGit.GUI.Elements
import           HGit.GUI.State
import           HGit.GUI.Messages
import           HGit.Generic.BlakeHash
import           HGit.Generic.RecursionSchemes
import           HGit.Generic.HRecursionSchemes
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
  -> Fix (ErrorAnnotatedMergeTrie UI) `Either` Fix (MergeTrie UI)
  -> UI Element
browseMergeTrie modifyMergeTrieHandler focusHandler _minimizations ipcOrC eroot
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

-- construct a NEL from a list and an element,
-- but with the element appended to the list instead of prepended
appendNEL :: [a] -> a -> NonEmpty a
appendNEL xs x = maybe (pure x) (<> pure x) $ nonEmpty xs


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
        liftIO $ popRequest $ SpawnRequestText "commit message" $ \msg -> liftIO $ do
          liftIO $ modifyMergeTrieHandler $ Finalize msg
      pure finalize

    resetCommit = do
      reset <- UI.button # set text "reset commit [!]"
      on UI.click reset $ \() -> void $ do
        liftIO $ modifyMergeTrieHandler Reset
      pure reset

    viewParents ps = do
      elems <- toList <$> traverse viewParent ps
      faUl #+ fmap element elems

    viewParent wipt = faLi focusHandler wipt [] (string "parent commit") UI.div

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
  _ <- set UI.title "chibi mononoke!" (pure root)
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

  initCommit <- expandHash (sRead blobStore) <$> uploadM (sWrite blobStore) commit1
  branchCommit <- expandHash (sRead blobStore) <$> uploadM (sWrite blobStore) commit2

  let initialBranchState = BranchState
                         { bsMainBranch = initCommit
                         , bsBranches = [("branch", branchCommit)]
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
          _ <- element modalRoot # set children []
          -- NOTE: this must be _after_ the above line, b/c it may spawn subsequent modal dialogs
          act input
        _ <- element modalRoot #+ [drawModal s [ string "req: "
                                               , element inputElem
                                               ]
                                  ]
        UI.setFocus inputElem


  let popError :: NonEmpty MergeError -> UI ()
      popError e = liftIO $ popupHandler $ SpawnError $ show e

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

        emt <- case mcommit of
          Nothing -> do
            -- redraw commit editor
            bs <- liftIO $ atomically $ readTVar branchState
            lift $ redrawSidebar bs Nothing

            commit <-  liftIO $ atomically $ snd <$> getCurrentBranch
            snap' <- updateSnapshotIndexLMMT blobStore commitSnapshotIndex commit
            (HC (Tagged _ snap)) <- lift $ fetchLMMT snap'
            let ft = unmodifiedWIP $ extractFT snap
            mt <- lift $ buildMergeTrie emptyMergeTrie ft
            pure $ Right mt -- no merge errors to render, if no in progress commit

          Just ipc@InProgressCommit{..} -> do
            -- redraw commit editor
            bs <- liftIO $ atomically $ readTVar branchState
            lift $ redrawSidebar bs $ Just ipc

            let changes = uncurry Change <$> Map.toList ipcChanges

            mt <- fmap snd $ makeMT changes ipcParentCommits (iRead commitSnapshotIndex) (sRead blobStore)

            case resolveMergeTrie' (modifiedWIP $ Commit "[wip placeholder]" changes ipcParentCommits) mt of
              Left e  -> pure $ Left e
              Right _ -> pure $ Right mt

        lift $ redrawMergeTrie emt
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
                                                 }
          writeTVar inProgressCommitTVar nextCommit
          pure nextCommit

      handleMMTE' Reset = liftIO $ atomically $ do
          let nextCommit = Nothing
          writeTVar inProgressCommitTVar nextCommit
          pure nextCommit

      handleMMTE' (Finalize msg) = do
          -- FIXME: interleaving STM and IO actions here, b/c need to upload commit. janky?
          mipc <- liftIO $ atomically $ do
            readTVar inProgressCommitTVar
          case mipc of
            Nothing  -> pure Nothing -- no-op, nothing to finalize
            Just InProgressCommit{..} -> do
              let changes = uncurry Change <$> Map.toList ipcChanges
                  commit = modifiedWIP $ Commit msg changes ipcParentCommits

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


  liftIO $ focusChangeHandler $ wrapFocus sing $ unmodifiedWIP initCommit

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

