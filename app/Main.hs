{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Main where

--------------------------------------------
import qualified Clay as Clay
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import qualified Data.Char as Char
import           Data.Functor.Compose
import           Data.List (intersperse)
import           Data.List.NonEmpty (toList, nonEmpty, (<|), NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import           Data.List.Split (splitOn)
import           Data.Text.Lazy (pack, unpack)
import           Data.Singletons.TH (SingI, sing)
--------------------------------------------
import           HGit.Core.Types
import           HGit.Core.MergeTrie
import           HGit.GUI.CSS
import           Merkle.Types.BlakeHash
import           Util.RecursionSchemes
import           Util.HRecursionSchemes as HR -- YOLO 420 SHINY AND CHROME
--------------------------------------------

import qualified Clay.Flexbox as CFB
import Control.Monad (void)
import Control.Concurrent.STM
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Ext.Flexbox


type Expansions = Set RawBlakeHash

data Focus x
  = SnapshotF (x 'SnapshotT)
  | FileTreeF (x 'FileTree)
  | CommitF   (x 'CommitT)
  | BlobF     (x 'BlobT)

type FocusWIPT m = Focus (WIPT m)
type FocusLMMT m = Focus (LMMT m)

-- (sing :: Sing i)
wrapFocus :: forall (i :: MTag) x. Sing i -> x i -> Focus x
wrapFocus s x = case s of
  SSnapshotT -> SnapshotF x
  SFileTree  -> FileTreeF x
  SCommitT   -> CommitF   x
  SBlobT     -> BlobF     x

data BranchState m
  = BranchState
  { bsMainBranch :: LMMT m 'CommitT
  , bsBranches :: [(String, LMMT m 'CommitT)]
  , bsFocus :: BranchFocus
  }


-- TODO: need to figure out how to _update_ branch - maybe just fetch commit from tvar on click instead of baking in value?
branchBrowser
  :: Index UI
  -> Store UI
  -> BranchState UI
  -> Handler (FocusWIPT UI)
  -> Handler UpdateBranchState
  -> UI Element
branchBrowser commitSnapshotIndex store bs focusChangeHandler updateBranchStateHandler = do
    branchList <- UI.ul
    drawBranch branchList (MainBranch, bsMainBranch bs)
    _ <- traverse (drawBranch branchList . (\(f,c) -> (OtherBranch f, c))) (bsBranches bs)

    addBranch <- UI.input
    on UI.sendValue addBranch $ \input -> do
      liftIO $ updateBranchStateHandler $ AddBranch input

    UI.div #+ [ element branchList
              , branchSelector
              , UI.br
              , string "add branch:"
              , element addBranch
              ]
  where
    branchSelector = do
      let branches = [(MainBranch, bsMainBranch bs)] ++ (fmap (\(x,y) -> (OtherBranch x, y)) $ bsBranches bs)
          options = fmap mkOption branches
          mkOption (f, c) = do
            let s = case f of
                  OtherBranch s -> s
                  MainBranch -> "main"
            opt <- UI.option # set UI.value s # set text s
            on UI.click opt $ \() -> void $ do
              liftIO $ updateBranchStateHandler $ ChangeFocus f
            pure opt
      UI.select #+ options

    drawBranch pn (f, commit) = do
      let branchName = case f of
            MainBranch -> "main"
            OtherBranch s -> s
      let branchName' = if f == bsFocus bs then ("[focus] -> " ++ branchName) else branchName
      thisBranch <- UI.li

      focus  <- focusButton focusChangeHandler (unmodifiedWIP commit)

      _ <- element thisBranch #+ [string branchName', element focus]

      case f of
        MainBranch -> pure ()
        OtherBranch s -> do
          deleteButton <- UI.button # set UI.class_ "delete-branch" # set UI.text "[delete]"
          on UI.click deleteButton $ \() -> do
            -- remove via branch name, simple/easy
            liftIO $ updateBranchStateHandler $ DelBranch branchName
          _ <- element thisBranch #+ [element deleteButton]
          pure ()

      snap <- updateSnapshotIndexLMMT store commitSnapshotIndex commit
      focusSnap  <- focusButton focusChangeHandler (unmodifiedWIP snap)

      _ <- element thisBranch #+ [element focusSnap]


      element pn #+ [element thisBranch]


data UpdateBranchState
  = AddBranch String -- branch off of current focus
  | DelBranch String
  | ChangeFocus BranchFocus -- blocked if IPC is Just

data BranchFocus
  = MainBranch
  | OtherBranch String
  deriving (Eq, Ord, Show)

data UpdateMergeTrie m
  = ApplyChange (Change (WIPT m))
  | AddParent (LMMT m 'CommitT)
  | Reset
  | Finalize

instance Show (UpdateMergeTrie m) where
  show (ApplyChange c) = "ApplyChange: " ++ (unlines $ renderChange $ cmapShim (Const . renderWIPT) c)
  show (AddParent _) = "AddParent: todo"
  show Reset = "Reset"
  show Finalize = "Finalize"


-- TODO: go through this and remove parent-node idiom
-- TODO/FIXME: full custom rendering for merge trie - needs to elide last changed commit (except for 'focus-on' link mb)
browseMergeTrie
  :: Handler (UpdateMergeTrie UI)
  -> Handler (FocusWIPT UI)
  -> TVar Expansions
  -> Fix (MergeTrie UI)
  -> UI Element
browseMergeTrie modifyMergeTrieHandler focusHandler expansions root = do
    (cata f root) []
  where
    f :: MergeTrie UI ([Path] -> UI Element) -> [Path] -> UI Element
    f mt path = do
      case mtChange mt of
        Just c -> do
          children <- renderChildren path $ mtChildren mt
          files <- renderFiles $ Map.toList $ mtFilesAtPath mt
          change <- UI.div #+ [string "change: ", renderChange c]
          -- TODO: remove_change via UI
          UI.div #+ fmap element [change, files, children]
        Nothing -> do
          children <- renderChildren path $ mtChildren mt
          files <- renderFiles $ Map.toList $ mtFilesAtPath mt

          case nonEmpty path of
            Nothing -> UI.div #+ fmap element [files, children]
            Just nepath -> do
              addChange <- UI.input
              on UI.sendValue addChange $ \input -> do
                liftIO $ modifyMergeTrieHandler $ ApplyChange $ add nepath $ modifiedWIP $ Blob input

              delChange <- UI.button # set UI.class_ "make-delete-change" # set UI.text "[delete]"
              on UI.click delChange $ \input -> do
                liftIO $ modifyMergeTrieHandler $ ApplyChange $ del nepath

              UI.div #+ fmap element [addChange, delChange, files, children]

    renderFiles fs = UI.ul #+ fmap renderFile fs

    renderFile _ = string "todo: render file"

    renderChange :: ChangeType (WIPT UI) -> UI Element
    renderChange (Add wipt) = do
      x <- UI.div
      (HC (Tagged _ blob)) <- fetchWIPT wipt

      focus <- focusButton focusHandler wipt

      let shim :: forall w. M w 'BlobT -> String
          shim (Blob blobstr) = blobstr
      let blobstr = shim blob

      element x #+ [string "ADD", element focus, string blobstr]
    renderChange Del = string "DELETE"


    renderChildren path c = UI.ul #+ fmap (renderChild path) (Map.toList c)

    renderChild :: [Path] -> (Path, WIPT UI 'FileTree `Either` ([Path] -> UI Element)) -> UI Element
    renderChild path (pathSegment, Left wipt) = do
      -- TODO: code tag for path segment?
      UI.li #+ [ focusButton focusHandler wipt
               , UI.code # set text pathSegment # set UI.class_ "path-segment"
               , string ": "
               , renderWIPTFileTree path pathSegment wipt
               ]

    renderChild path (pathSegment, Right next) = do
      UI.li #+ [next (path ++ [pathSegment])]


    -- returns fieldset
    renderWIPTBlob :: [Path] -> WIPT UI 'BlobT -> UI Element
    renderWIPTBlob path wipt = do
      (HC (Tagged h blob)) <- fetchWIPT wipt
      case blob of
        Blob b -> do
          string b

    -- returns fieldset
    renderWIPTFileTree :: [Path] -> Path -> WIPT UI 'FileTree -> UI Element
    renderWIPTFileTree path pathSegment wipt = do
      (HC (Tagged h ft)) <- fetchWIPT wipt
      case ft of
        Dir cs -> do
          let cs' = (uncurry (renderWIPTFileTree $ path ++ [pathSegment]) <$> Map.toList cs)
              cs'' = UI.ul #+ (f <$> Map.toList cs)
              f (p,c) = UI.li #+ [ focusButton focusHandler c
                                 , UI.code # set text p # set UI.class_ "path-segment"
                                 , string ": "
                                 , renderWIPTFileTree (path ++ [pathSegment]) p c
                                 ]
          cs''
        File blob commit prev -> do
          -- TODO: focus button for prev version(s)
          UI.div #+ [ UI.ul #+ ( [ UI.li #+ [ string "from commit: "
                                           , focusButton focusHandler commit
                                           ]
                                 ] ++
                                 (fmap (\x -> UI.li #+ [string "prev iteration: ", focusButton focusHandler x]) prev) ++
                                 [UI.li #+ [string "blob: "
                                           , focusButton focusHandler blob
                                           , renderWIPTBlob (path ++ [pathSegment]) blob
                                           ]
                                 ]
                               )
                    ]

drawCommitEditor
  :: BranchState UI
  -> Handler (UpdateMergeTrie UI)
  -> Handler (FocusWIPT UI)
  -> Maybe InProgressCommit
  -> UI Element
drawCommitEditor bs modifyMergeTrieHandler focusHandler ipc = do

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
      UI.ul #+ fmap element elems

    viewParent wipt = do
      (HC (Tagged _ m)) <- fetchWIPT wipt
      case m of
        NullCommit -> UI.li #+ [string "nullcommit"]
        Commit msg _ _ -> UI.li #+ [ string ("commit: \"" ++ msg ++ "\"")
                                   , focusButton focusHandler wipt
                                   ]

    viewMessage msg = string ("msg: \"" ++ msg ++  "\"")



    viewChanges cs = do
      UI.ul #+ fmap viewChange cs
    renderPath = mconcat . intersperse "/" . toList
    viewChange Change{..} = case _change of
      Add a -> UI.li #+ [string ("add: " ++ renderPath _path), focusButton focusHandler a]
      Del -> UI.li #+ [string ("del: " ++ renderPath _path)]

    addChanges = do
      add <- UI.button # set text "add"
      del <- UI.button # set text "del"

      pathInput     <- UI.input
      contentsInput <- UI.input

      on UI.click del $ \() -> void $ runMaybeT $ do
        path <- lift (UI.get UI.value pathInput) >>= MaybeT . pure . parsePath
        liftIO $ modifyMergeTrieHandler (ApplyChange $ Change path Del)

      on UI.click add $ \() -> void $ runMaybeT $ do
        path <- lift (UI.get UI.value pathInput) >>= MaybeT . pure . parsePath
        contents <- lift $ UI.get UI.value contentsInput
        liftIO $ modifyMergeTrieHandler (ApplyChange $ Change path $ Add $ modifiedWIP $ Blob contents)

      UI.div #+ [ string "path:"
                , element pathInput
                , UI.br
                , string "contents:"
                , element contentsInput
                , UI.br
                , element del
                , element add
                ]


-- TODO: TEST THIS <- also, make errors (eg change to node with no file) visible via merge trie type (?)
parsePath :: String -> Maybe (NonEmpty Path)
parsePath s = do
  case nonEmpty (filter (/= "") $ splitOn "/" s) of
    Nothing -> Nothing
    Just xs -> Just xs


browseWIPT
  :: Handler (FocusWIPT UI)
  -> FocusWIPT UI
  -> UI Element
browseWIPT focusHandler focus = case focus of
    SnapshotF root -> (getConst $ hpara (uiWIPAlg focusHandler) root)
    FileTreeF root -> (getConst $ hpara (uiWIPAlg focusHandler) root)
    CommitF   root -> (getConst $ hpara (uiWIPAlg focusHandler) root)
    BlobF     root -> (getConst $ hpara (uiWIPAlg focusHandler) root)


browseLMMT
  :: Handler (FocusWIPT UI)
  -> FocusLMMT UI
  -> UI Element
browseLMMT focusHandler focus = case focus of
    SnapshotF root -> (getConst $ hpara (uiLMMAlg focusHandler) root)
    FileTreeF root -> (getConst $ hpara (uiLMMAlg focusHandler) root)
    CommitF   root -> (getConst $ hpara (uiLMMAlg focusHandler) root)
    BlobF     root -> (getConst $ hpara (uiLMMAlg focusHandler) root)


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
  -> RAlg (WIP UI) (Const (UI Element))
uiWIPAlg focusHandler (HC (L lmmt)) = Const $ do
      browseLMMT focusHandler (wrapFocus sing $ lmmt)
uiWIPAlg focusHandler wipt@(HC (R m)) = Const $ do
      let focus = focusButton focusHandler $ Term $ hfmap _tag wipt
      getConst $ browseMononoke focus ["wip"] $ hfmap _elem m


uiLMMAlg
  :: Handler (FocusWIPT UI)
  -> RAlg (LMM UI) (Const (UI Element))
uiLMMAlg focusHandler lmm = Const $ do
      m <-  fetchLMM lmm
      let focus = focusButton focusHandler $ unmodifiedWIP $ Term $ hfmap _tag lmm
      getConst $ browseMononoke focus ["persisted"] $ hfmap _elem m


browseMononoke :: UI Element -> [String] -> Alg (Tagged Hash `HCompose` M) (Const (UI Element))
browseMononoke focus extraTags (HC (Tagged h m)) = Const $ do
      wrapper <- UI.fieldset # set UI.class_ (mconcat $ intersperse " " $ extraTags ++ [typeTagName' m, "node"])

      hdr <- UI.legend # set text (typeTagName' m)
                       #+ [ focus ]
      element wrapper #+ ([element hdr] ++ browseMononoke' m)


  where
    renderChange :: Change (Const (UI Element)) -> UI Element
    renderChange Change{..} =
        let renderPath = mconcat . intersperse "/" . toList
        in case _change of
              Del -> UI.li #+ [string $ renderPath _path ++ ": Del"]
              Add (Const blob) ->
                UI.li #+ [UI.string (renderPath _path ++ ": Add: "), blob]

    browseMononoke' :: forall (x:: MTag). M (Const (UI Element)) x -> [UI Element]
    browseMononoke' (Snapshot t o ps) =
      [ getConst t
      , getConst o
      , do
          parentList <- nestedUL #+ ((UI.li #+) . pure . getConst <$> ps)
          UI.div #+ [string "parents:", element parentList]
      ]

    browseMononoke' (File b l p) =
      [ getConst b
      , getConst l
      , nestedUL #+ ((UI.li #+) . pure . getConst <$> p)
      ]

    browseMononoke' (Dir cs) =
      [ do
          let renderChild (k, Const v) = UI.li #+ [string (k ++ ":"), v]
          nestedUL #+ (renderChild <$> Map.toList cs)
      ]

    browseMononoke' NullCommit =
      [ string "NullCommit"
      ]

    browseMononoke' (Commit m cs ps) =
      [ UI.string $ "msg: " ++ m
      , labelDiv "CHANGES" [nestedUL #+ (renderChange <$> cs)]
      , do
          parentList <- nestedUL #+ ((UI.li #+) . pure . getConst <$> toList ps)
          labelDiv "PARENTS" [element parentList]
      ]

    browseMononoke' (Blob c) =
      [ UI.string c
      ]



nestedUL :: UI Element
nestedUL = UI.ul # set (attr "style") "margin-left:1em"


nestedDiv :: UI Element
nestedDiv = UI.div # set (attr "style") "margin-left:1em"

fileTreeEntity :: [UI Element] -> [UI Element] -> UI Element
fileTreeEntity s x = UI.fieldset
                   # set UI.class_ "filetree-viewer"
                   #+ ([UI.legend #+ s # set UI.class_ "filetree-viewer-legend"] ++ x)


labelDiv :: String -> [UI Element] -> UI Element
labelDiv s x = UI.fieldset # set UI.class_ "vertical-legend"
                           #+ [UI.legend # set text s # set UI.class_ "vertical-legend", nestedDiv #+ x]


main :: IO ()
main = do
  startGUI (defaultConfig { jsStatic = Just "static"}) setup


-- NOTE/TODO: this could all be in a single STM transaction. wild.
updateSnapshotIndexLMMT :: MonadIO m => Store m -> Index m -> LMMT m 'CommitT -> m (LMMT m 'SnapshotT)
updateSnapshotIndexLMMT store index commit = do
  msnap <- (iRead index) (hashOfLMMT commit)
  (HC (Tagged _ commit')) <- fetchLMMT commit
  case msnap of
    Just h  -> do
      pure $ expandHash (sRead store) h
    Nothing -> do
      esnap <- runExceptT $ makeSnapshot (hfmap unmodifiedWIP commit') (iRead index) (sRead store)
      -- FIXME
      snap <- either undefined pure esnap
      let wipt = modifiedWIP snap
      uploadedSnap <- uploadWIPT (sWrite store) wipt
      (iWrite index) (hashOfLMMT commit) (hashOfLMMT uploadedSnap)
      pure uploadedSnap


updateSnapshotIndexWIPT :: MonadIO m => StoreRead m -> IndexRead m -> WIPT m 'CommitT -> m (M (WIPT m) 'SnapshotT)
updateSnapshotIndexWIPT store index commit = do
  (HC (Tagged _ commit')) <- fetchWIPT commit
  esnap <- runExceptT $ makeSnapshot commit' index store
  snap <- either undefined pure esnap
  pure snap

data InProgressCommit
  = InProgressCommit
  { ipcMsg           :: String
  , ipcChanges       :: [Change (WIPT UI)]
  , ipcParentCommits :: NonEmpty (WIPT UI 'CommitT)
  }


setup :: Window -> UI ()
setup root = void $ do
  getHead root #+ [ mkElement "style"
          # set (UI.text) (unpack (Clay.render css))
          ]
  UI.addStyleSheet root "all.css" -- fontawesome


  commitSnapshotIndexTVar <- liftIO . atomically $ newTVar Map.empty
  let commitSnapshotIndex = stmIOIndex commitSnapshotIndexTVar

  inProgressCommitTVar :: TVar (Maybe InProgressCommit)
    <- liftIO . atomically $ newTVar $ Nothing

  blobStoreTvar <- liftIO . atomically $ newTVar emptyBlobStore
  let blobStore = stmIOStore blobStoreTvar


  initCommitHash <- uploadM (sWrite blobStore) $ Term NullCommit
  let innitCommit = expandHash (sRead blobStore) initCommitHash

  let initialBranchState = BranchState
                         { bsMainBranch = innitCommit
                         , bsBranches = []
                         , bsFocus = MainBranch
                         }


  branchState <- liftIO . atomically $ newTVar initialBranchState
  expansions <- liftIO . atomically $ newTVar (Set.empty)

  -- errors on key not found - not sure how to handle this, need
  -- generic 'shit broke' error channel, eg popup/alert
  let getCurrentBranch :: STM (BranchFocus, LMMT UI 'CommitT)
      getCurrentBranch = do
        bs <- readTVar branchState
        let focus = bsFocus bs
        case focus of
          MainBranch -> pure $ (focus, bsMainBranch bs)
          OtherBranch b -> pure $ maybe (error "todo") (focus, ) $ lookup b (bsBranches bs)

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


  -- empty root nodes for various UI elements
  branchBrowserRoot <- simpleDiv
  mergeTrieRoot <- simpleDiv
  commitEditorRoot <- simpleDiv

  let extractFT :: M x 'SnapshotT -> x 'FileTree
      extractFT (Snapshot ft _ _) = ft

  let redrawCommitEditor bs mipc = do
        _ <- element commitEditorRoot # set children []
        element commitEditorRoot #+ [drawCommitEditor bs modifyMergeTrieHandler focusChangeHandler mipc]

  let redrawBranchBrowser bs = do
        _ <- element branchBrowserRoot # set children []
        element branchBrowserRoot #+ [branchBrowser commitSnapshotIndex blobStore bs focusChangeHandler updateBranchStateHandler]


  let redrawMergeTrie mt = void $ do
        _ <- element mergeTrieRoot # set children []
        element mergeTrieRoot #+ [browseMergeTrie modifyMergeTrieHandler focusChangeHandler expansions mt]

  let handleMMTE msg = do
        _ <- element mergeTrieRoot # set children []
        liftIO $ print $ "handle MMTE msg"
        liftIO $ print $ show msg
        mcommit <- handleMMTE' msg

        ft <- case mcommit of
          Nothing -> do
            -- redraw commit editor
            bs <- liftIO $ atomically $ readTVar branchState
            redrawCommitEditor bs Nothing

            commit <-  liftIO $ atomically $ snd <$> getCurrentBranch
            snap' <- updateSnapshotIndexLMMT blobStore commitSnapshotIndex commit
            (HC (Tagged _ snap)) <- fetchLMMT snap'
            pure $ unmodifiedWIP $ extractFT snap
          Just ipc@InProgressCommit{..} -> do
            -- redraw commit editor
            bs <- liftIO $ atomically $ readTVar branchState
            redrawCommitEditor bs $ Just ipc

            let commit = Commit ipcMsg ipcChanges ipcParentCommits
            -- FIXME
            esnap <- runExceptT $ makeSnapshot commit (iRead commitSnapshotIndex) (sRead blobStore)
            snap <- either undefined pure esnap
            pure $ extractFT snap

        mt <- buildMergeTrie emptyMergeTrie ft
        redrawMergeTrie mt
        pure ()

      handleMMTE' :: UpdateMergeTrie UI -> UI (Maybe InProgressCommit)
      handleMMTE' (ApplyChange change) = liftIO $ atomically $ do
          c <- readTVar inProgressCommitTVar
          nextCommit <- case c of
                (Just ipc) -> pure $ Just $ ipc { ipcChanges = ipcChanges ipc ++ [change] }
                Nothing ->    do
                  commit <- snd <$> getCurrentBranch
                  pure $ Just $ InProgressCommit { ipcChanges = [change]
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
                  pure $ Just $ InProgressCommit { ipcChanges = []
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
        -- TODO: write commit to store, update bs, redrawBranchBrowser bs'
          -- FIXME: interleaving STM and IO actions here, b/c need to upload commit. janky?
          mipc <- liftIO $ atomically $ do
            readTVar inProgressCommitTVar
          case mipc of
            Nothing  -> pure Nothing -- no-op, nothing to finalize
            Just InProgressCommit{..} -> do
              let commit = modifiedWIP $ Commit ipcMsg ipcChanges ipcParentCommits
              uploadedCommit <- uploadWIPT (sWrite blobStore) commit

              bs <- liftIO $ atomically $ do
                -- assertion: IPC and current branch are always intertwined, so this is safe
                updateCurrentBranch uploadedCommit
                writeTVar inProgressCommitTVar Nothing
                bs <- readTVar branchState
                pure bs

              redrawBranchBrowser bs

              pure Nothing


  -- discarded return value deregisters handler
  _ <- onEvent modifyMergeTrieEvent handleMMTE

  liftIO $ modifyMergeTrieHandler $ Reset

  browserRoot <- simpleDiv
  -- discarded return value deregisters handler
  _ <- onEvent focusChangeEvent $ \focus -> do
    _ <- element browserRoot # set children []
    element browserRoot #+ [browseWIPT focusChangeHandler focus]
    pure ()


  liftIO $ focusChangeHandler $ wrapFocus sing $ unmodifiedWIP innitCommit

  _ <- onEvent updateBranchStateEvent $ \ubs -> do
    bs' <- case ubs of
      AddBranch s -> do
        liftIO $ print "add branch"
        liftIO $ atomically $ do
          bs <- readTVar branchState
          currentBranchPersistedCommit <- snd <$> getCurrentBranch

          let bs' = bs { bsBranches = bsBranches bs ++ [(s, currentBranchPersistedCommit)] }
          writeTVar branchState bs'
          pure bs'
      DelBranch s -> do
        liftIO $ print "del branch: todo"
        liftIO $ atomically $ do
          bs <- readTVar branchState
          -- TODO: error if target not found in list of branches
          -- let bs' = bs { bsFocus = f }
          -- writeTVar branchState bs'
          pure bs
      ChangeFocus f -> do
        liftIO $ print "changefocus"
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

    redrawBranchBrowser bs'
    pure ()

  bs <- liftIO $ atomically $ readTVar branchState
  element branchBrowserRoot #+ [branchBrowser commitSnapshotIndex blobStore bs focusChangeHandler updateBranchStateHandler]

  sidebar <- flex UI.div (flexDirection CFB.column)
                [ (element commitEditorRoot, flexGrow 1)
                , (element branchBrowserRoot, flexGrow 1)
                ]

  flex_p (getBody root) [ (element sidebar, flexGrow 1)
                        , (element mergeTrieRoot, flexGrow 2)
                        , (element browserRoot, flexGrow 2)
                        ]

-- TODO: use clay directly, would greatly simplify CSS work here
-- | Simple coloured 'div'
simpleDiv :: UI Element
simpleDiv = UI.div # set UI.style
          [ ("background-color", "#F89406")
          , ("margin", "8px")
          ]
