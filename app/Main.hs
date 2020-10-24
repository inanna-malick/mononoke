{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Main where

--------------------------------------------
import qualified Clay as Clay
import           Data.Functor.Compose
import           Data.List (intersperse)
import           Data.List.NonEmpty (toList, nonEmpty, (<|), NonEmpty(..))
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

import Control.Monad (void)
import Control.Concurrent.STM
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Ext.Flexbox

data CommitFocus = MainBranch | OtherCommit String

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
  }


-- TODO: need to figure out how to _update_ branch - maybe just fetch commit from tvar on click instead of baking in value?
branchBrowser
  :: Element
  -> Index UI
  -> Store UI
  -> TVar (BranchState UI)
  -> Handler (FocusWIPT UI)
  -> UI Element
branchBrowser parentElement commitSnapshotIndex store branchState focusChangeHandler = do
    liftIO $ print "branch browser"
    bs <- liftIO $ atomically $ readTVar branchState
    branchList <- UI.ul
    drawBranch False branchList ("main", bsMainBranch bs)
    _ <- traverse (drawBranch True branchList) (bsBranches bs)

    addBranch <- UI.input
    on UI.sendValue addBranch $ \input -> do
      let nullCommit = NullCommit
      (sWrite store) nullCommit
      let branch = (input, liftLMMT $ Term nullCommit)
      liftIO $ atomically $ modifyTVar branchState $ \bs ->
        bs { bsBranches = bsBranches bs ++ [branch] }
      drawBranch True branchList branch

    element parentElement #+ [ element branchList
                             , string "add branch:"
                             , element addBranch
                             ]
  where
    drawBranch del pn (branchName, commit) = do
      thisBranch <- UI.li

      focus  <- UI.button # set UI.class_ "focus-commit" # set UI.text "[commit]"
      on UI.click focus $ \() -> do
        -- TODO: get from tvar - commit may change w/o regening this element? idk mb
        liftIO $ focusChangeHandler (CommitF $ unmodifiedWIP commit)


      _ <- element thisBranch #+ [string branchName, element focus]

      if not del then pure () else do
        deleteButton <- UI.button # set UI.class_ "delete-branch" # set UI.text "[delete]"
        on UI.click deleteButton $ \() -> do
          -- remove via branch name, simple/easy
          liftIO $ atomically $ modifyTVar branchState $ \bs ->
            bs { bsBranches = filter (not . (== branchName) . fst) $ bsBranches bs }
          delete thisBranch
        _ <- element thisBranch #+ [element deleteButton]
        pure ()

      snap <- updateSnapshotIndexLMMT store commitSnapshotIndex commit
      focusSnap <- UI.button # set UI.class_ "focus-snap" # set UI.text "[snapshot]"
      on UI.click focusSnap $ \() -> do
        liftIO $ focusChangeHandler (SnapshotF $ unmodifiedWIP snap)

      _ <- element thisBranch #+ [element focusSnap]


      element pn #+ [element thisBranch]



data UpdateMergeTrie m
  = ApplyChange (Change (WIPT m))
  | AddParent (LMMT m 'CommitT)
  | Reset




-- TODO/FIXME: full custom rendering for merge trie - needs to elide last changed commit (except for 'focus-on' link mb)
browseMergeTrie
  :: Handler (UpdateMergeTrie UI)
  -> Handler (FocusWIPT UI)
  -> TVar Expansions
  -> Fix (MergeTrie UI)
  -> Element
  -> UI Element
browseMergeTrie modifyMergeTrieHandler focusHandler expansions root parentElement = do
    liftIO $ print "browse merge trie"
    (cata f root) [] parentElement
  where
    f :: MergeTrie UI ([Path] -> Element -> UI Element) -> [Path] -> Element -> UI Element
    f mt path pn = do
      liftIO $ print "browse merge trie: f"
      case mtChange mt of
        Just c -> do
          children <- renderChildren path $ mtChildren mt
          files <- renderFiles $ mtFilesAtPath mt
          change <- UI.div #+ [string "change: ", renderChange c]
          -- TODO: remove_change via UI
          element pn #+ fmap element [change, files, children]
        Nothing -> do
          children <- renderChildren path $ mtChildren mt
          files <- renderFiles $ mtFilesAtPath mt

          case nonEmpty path of
            Nothing -> element pn #+ fmap element [files, children]
            Just nepath -> do
              addChange <- UI.input
              on UI.sendValue addChange $ \input -> do
                liftIO $ modifyMergeTrieHandler $ ApplyChange $ add nepath $ modifiedWIP $ Blob input

              delChange <- UI.button # set UI.class_ "make-delete-change" # set UI.text "[delete]"
              on UI.click delChange $ \input -> do
                liftIO $ modifyMergeTrieHandler $ ApplyChange $ del nepath

              element pn #+ fmap element [addChange, delChange, files, children]

    renderFiles fs = do
      liftIO $ print "browse merge trie: renderFiles"
      pn <- UI.ul
      traverse (renderFile pn) fs
      element pn

    renderFile pn _ = element pn #+ [string "todo: render file"]

    renderChange :: ChangeType (WIPT UI) -> UI Element
    renderChange (Add wipt) = do
      liftIO $ print "browse merge trie: renderChange add"
      x <- UI.div
      (HC (Tagged _ blob)) <- fetchWIPT wipt

      focus <- UI.button # set UI.class_ "focus-small" # set UI.text "<>"
      on UI.click focus $ \() -> do
        liftIO $ focusHandler $ BlobF wipt

      let shim :: forall w. M w 'BlobT -> String
          shim (Blob blobstr) = blobstr
      let blobstr = shim blob

      element x #+ [string "ADD", element focus, string blobstr]
    renderChange Del = string "DELETE"


    renderChildren path c = do
      liftIO $ print "browse merge trie: renderChildren"
      pn <- UI.ul
      traverse (renderChild path pn) $ Map.toList c
      element pn

    renderChild :: [Path] -> Element -> (Path, WIPT UI 'FileTree `Either` ([Path] -> Element -> UI Element)) -> UI Element
    renderChild path pn (pathSegment, Left wipt) = do
      -- TODO: code tag for path segment?
      element pn #+ [renderWIPTFileTree path pathSegment wipt]

    renderChild path pn (pathSegment, Right next) = do
      element pn #+ [UI.li >>= next (path ++ [pathSegment])]


    -- returns fieldset
    renderWIPTBlob :: [Path] -> WIPT UI 'BlobT -> UI Element
    renderWIPTBlob path wipt = do
      (HC (Tagged h blob)) <- fetchWIPT wipt
      case blob of
        Blob b -> do
          labelDiv "blob" [string b]

    -- returns fieldset
    renderWIPTFileTree :: [Path] -> Path -> WIPT UI 'FileTree -> UI Element
    renderWIPTFileTree path pathSegment wipt = do
      (HC (Tagged h ft)) <- fetchWIPT wipt
      case ft of
        Dir cs -> do
          labelDiv (pathSegment ++ "/") $ uncurry (renderWIPTFileTree $ path ++ [pathSegment]) <$> Map.toList cs
        File blob commit _prev -> do
          -- TODO: focus button for prev version(s)
          labelDiv (pathSegment ++ "/") [ focusButton focusHandler wipt
                                        , focusButton focusHandler commit
                                        , renderWIPTBlob (path ++ [pathSegment]) blob
                                        ]




browseWIPT
  :: Handler (FocusWIPT UI)
  -> TVar Expansions
  -> FocusWIPT UI
  -> Element
  -> UI Element
browseWIPT focusHandler expansions focus parentElement = case focus of
    SnapshotF root -> (getConst $ hpara (uiWIPAlg focusHandler expansions) root) parentElement
    FileTreeF root -> (getConst $ hpara (uiWIPAlg focusHandler expansions) root) parentElement
    CommitF   root -> (getConst $ hpara (uiWIPAlg focusHandler expansions) root) parentElement
    BlobF     root -> (getConst $ hpara (uiWIPAlg focusHandler expansions) root) parentElement


browseLMMT
  :: Handler (FocusWIPT UI)
  -> TVar Expansions
  -> FocusLMMT UI
  -> Element
  -> UI Element
browseLMMT focusHandler expansions focus parentElement = case focus of
    SnapshotF root -> (getConst $ hpara (uiLMMAlg focusHandler expansions) root) parentElement
    FileTreeF root -> (getConst $ hpara (uiLMMAlg focusHandler expansions) root) parentElement
    CommitF   root -> (getConst $ hpara (uiLMMAlg focusHandler expansions) root) parentElement
    BlobF     root -> (getConst $ hpara (uiLMMAlg focusHandler expansions) root) parentElement


uiWIPAlg
  :: Handler (FocusWIPT UI)
  -> TVar Expansions
  -> RAlg (WIP UI) (Const (Element -> UI Element))
uiWIPAlg focusHandler expansions (HC (L lmmt)) = Const $ \pn -> do
  liftIO $ print "ui WIP alg: lmmt"
  browseLMMT focusHandler expansions (wrapFocus sing $ lmmt) pn
uiWIPAlg focusHandler expansions (HC (R (HC (Tagged h m)))) = Const $ \pn -> do -- TODO: proper WIPT UI, this is a hack
  -- (getConst $ uiMAlg $ HC $ Tagged h $ HC $ Compose $ pure $ hfmap _elem m) pn
  wrapper <- UI.fieldset # set (UI.class_) ("wip " ++ typeTagName' m ++ " node")

  (getConst $ uiMAlg $ hfmap _elem m) wrapper

  element pn #+ [element wrapper]

focusButton
  :: forall (x:: MTag)
   . SingI x
  => Handler (FocusWIPT UI)
  -> WIPT UI x
  -> UI Element
focusButton focusHandler wipt = do
    focus <- UI.button # set UI.class_ "focus-small" # set UI.text "<>"
    on UI.click focus $ \() -> do
      liftIO $ focusHandler $ wrapFocus sing wipt
    pure focus


uiLMMAlg
  :: Handler (FocusWIPT UI)
  -> TVar Expansions
  -> RAlg (LMM UI) (Const (Element -> UI Element))
uiLMMAlg focusHandler expansions x@(HC (Tagged (Const raw) (HC (Compose m)))) = Const $ \pn -> do
  let minimized = do
        maximize <- UI.button # set UI.class_ "expand-small" # set UI.text "+"
        on UI.click maximize $ \() -> do
          liftIO $ atomically $ modifyTVar expansions (Set.insert raw)
          _ <- set UI.children [] (element pn)
          maximized

        wrapper <- UI.fieldset # set (UI.class_) ("persisted " ++ typeTagName' x ++ " node")

        hdr <- UI.legend # set text (typeTagName' x) #+ [ focusButton focusHandler $ unmodifiedWIP (Term $ hfmap _tag x)
                                                        , element maximize
                                                        ]
        element wrapper # set text "..." #+ [element hdr]

        element pn #+ [element wrapper]

      maximized = do
        mm <- m
        minimize <- UI.button # set UI.class_ "minimize-small" # set UI.text "-"
        on UI.click minimize $ \() -> do
          liftIO $ atomically $ modifyTVar expansions (Set.delete raw)
          _ <- set UI.children [] (element pn)
          minimized

        wrapper <- UI.fieldset # set (UI.class_) ("persisted " ++ typeTagName' mm ++ " node")

        hdr <- UI.legend # set text (typeTagName' x) #+ [ focusButton focusHandler $ unmodifiedWIP (Term $ hfmap _tag x)
                                                        , element minimize
                                                        ]
        element wrapper #+ [element hdr]

        (getConst $ uiMAlg $ hfmap _elem mm) wrapper

        element pn #+ [element wrapper]

  isExpanded <- liftIO $ atomically $ Set.member raw <$> readTVar expansions
  case isExpanded of
    False -> minimized
    True  -> maximized


uiMAlg :: Alg M (Const (Element -> UI Element))
uiMAlg (Snapshot t o ps) = Const $ \pn -> do
    tree    <- UI.div >>= getConst t
    orig    <- UI.div >>= getConst o
    parentList <- nestedUL #+ ((UI.li >>= ) . getConst <$> ps)
    parents <- UI.div #+ [string "parents:", element parentList]
    element pn #+ fmap element [tree, orig, parents]

uiMAlg (File b l p)
  = Const $ \pn -> do
    blob    <- UI.div >>= getConst b
    lastMod <- UI.div >>= getConst l
    prev    <- nestedUL #+ ((UI.li >>= ) . getConst <$> p)
    element pn #+ fmap element [blob, lastMod, prev]

uiMAlg (Dir cs) = Const $ \pn -> do
    let renderChild (k, Const v) = UI.li #+ [string (k ++ ":"), UI.div >>= v]
    childrenList <- nestedUL #+ (renderChild <$> Map.toList cs)
    element pn #+ fmap element [childrenList]


uiMAlg NullCommit = Const $ \pn -> element pn #+ [ UI.legend # set text "NullCommit" ]

uiMAlg (Commit m cs ps) = Const $ \pn -> do
    msg <- UI.string $ "msg: " ++ m

    let renderPath = mconcat . intersperse "/" . toList
        renderChange Change{..} = case _change of
          Del -> UI.li #+ [string $ renderPath _path ++ ": Del"]
          Add (Const blob) ->
            UI.li #+ [UI.string (renderPath _path ++ ": Add: "), UI.div >>= blob]


    changes <- labelDiv "CHANGES" [nestedUL #+ (renderChange <$> cs)]

    parentList <- nestedUL #+ ((UI.li >>= ) . getConst <$> toList ps)
    parents <- labelDiv "PARENTS" [element parentList]

    element pn #+ fmap element [msg, changes, parents]

uiMAlg (Blob c) = Const $ \pn -> do
    content <- UI.string c
    element pn #+ fmap element [content]



nestedUL :: UI Element
nestedUL = UI.ul # set (attr "style") "margin-left:1em"


nestedDiv :: UI Element
nestedDiv = UI.div # set (attr "style") "margin-left:1em"

labelDiv :: String -> [UI Element] -> UI Element
labelDiv s x = UI.fieldset # set UI.class_ "vertical-legend"
                           #+ [UI.legend # set text s # set UI.class_ "vertical-legend", nestedDiv #+ x]


main :: IO ()
main = do
  startGUI defaultConfig setup


-- NOTE/TODO: this could all be in a single STM transaction. wild.
updateSnapshotIndexLMMT :: MonadIO m => Store m -> Index m -> LMMT m 'CommitT -> m (LMMT m 'SnapshotT)
updateSnapshotIndexLMMT store index commit = do
  msnap <- (iRead index) (hashOfLMMT commit)
  (HC (Tagged _ commit')) <- fetchLMMT commit
  case msnap of
    Just h  -> do
      pure $ expandHash (sRead store) h
    Nothing -> do
      snap <- makeSnapshot (hfmap unmodifiedWIP commit') (iRead index) (sRead store)
      let wipt = modifiedWIP snap
      uploadedSnap <- uploadWIPT (sWrite store) wipt
      (iWrite index) (hashOfLMMT commit) (hashOfLMMT uploadedSnap)
      pure uploadedSnap


updateSnapshotIndexWIPT :: MonadIO m => StoreRead m -> IndexRead m -> WIPT m 'CommitT -> m (M (WIPT m) 'SnapshotT)
updateSnapshotIndexWIPT store index commit = do
  (HC (Tagged _ commit')) <- fetchWIPT commit
  snap <- makeSnapshot commit' index store
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

  liftIO $ print "start setup"
  let initialBranchState = BranchState
                         { bsMainBranch = liftLMMT commit3
                         , bsBranches = []
                         }


  commitSnapshotIndexTVar <- liftIO . atomically $ newTVar Map.empty
  let commitSnapshotIndex = stmIOIndex commitSnapshotIndexTVar

  inProgressCommitTVar :: TVar (Maybe InProgressCommit)
    <- liftIO . atomically $ newTVar $ Nothing

  blobStoreTvar <- liftIO . atomically $ newTVar emptyBlobStore
  let blobStore = stmIOStore blobStoreTvar

  initCommitHash <- uploadM (sWrite blobStore) commit3
  let innitCommit = expandHash (sRead blobStore) initCommitHash

  -- mergeState <- liftIO . atomically $ newTVar mt
  branchState <- liftIO . atomically $ newTVar initialBranchState
  expansions <- liftIO . atomically $ newTVar (Set.empty)

  (focusChangeEvent, focusChangeHandler) <- liftIO $ newEvent
  (modifyMergeTrieEvent, modifyMergeTrieHandler) <- liftIO $ newEvent


  liftIO $ print "a"

  mergeTrieRoot <- simpleDiv

  let extractFT :: M x 'SnapshotT -> x 'FileTree
      extractFT (Snapshot ft _ _) = ft
  let handleMMTE msg = do
        liftIO $ print "handleMMTE"
        _ <- element mergeTrieRoot # set children []
        liftIO $ print "aa"
        mcommit <- liftIO $ atomically $ handleMMTE' msg
        liftIO $ print "bb"

        ft <- case mcommit of
          Nothing -> do
            -- TODO: handle arbitrary branch focus, currently just defaulting to main branch
            commit <- liftIO $ atomically $ bsMainBranch <$> readTVar branchState
            snap' <- updateSnapshotIndexLMMT blobStore commitSnapshotIndex commit
            (HC (Tagged _ snap)) <- fetchLMMT snap'
            pure $ unmodifiedWIP $ extractFT snap
          Just InProgressCommit{..} -> do
            let commit = Commit ipcMsg ipcChanges ipcParentCommits
            snap <- makeSnapshot commit (iRead commitSnapshotIndex) (sRead blobStore)
            pure $ extractFT snap

        mt <- buildMergeTrie emptyMergeTrie ft
        _ <- browseMergeTrie modifyMergeTrieHandler focusChangeHandler expansions mt mergeTrieRoot
        pure ()

      handleMMTE' :: UpdateMergeTrie UI -> STM (Maybe InProgressCommit)
      handleMMTE' (ApplyChange change) = do
          c <- readTVar inProgressCommitTVar
          let nextCommit = case c of
                (Just ipc) -> pure $ ipc { ipcChanges = ipcChanges ipc ++ [change] }
                Nothing -> undefined
          writeTVar inProgressCommitTVar nextCommit
          pure nextCommit

      handleMMTE' (AddParent parentToAdd) = do
          c <- readTVar inProgressCommitTVar
          let nextCommit = case c of
                (Just ipc) -> pure $ ipc { ipcParentCommits = unmodifiedWIP parentToAdd <| ipcParentCommits ipc }
                Nothing -> undefined
          writeTVar inProgressCommitTVar nextCommit
          pure nextCommit

      handleMMTE' Reset = do
          let nextCommit = Nothing
          writeTVar inProgressCommitTVar nextCommit
          pure nextCommit


  -- discarded return value deregisters handler
  _ <- onEvent modifyMergeTrieEvent handleMMTE

  liftIO $ print "b"

  liftIO $ modifyMergeTrieHandler $ Reset

  liftIO $ print "c"

  browserRoot <- simpleDiv
  -- discarded return value deregisters handler
  _ <- onEvent focusChangeEvent $ \focus -> do
    _ <- element browserRoot # set children []
    browseWIPT focusChangeHandler expansions focus browserRoot
    pure ()

  liftIO $ print "d"
  liftIO $ focusChangeHandler $ wrapFocus sing $ unmodifiedWIP innitCommit

  liftIO $ print "e"
  branchBrowserRoot <- simpleDiv
  branchBrowser branchBrowserRoot commitSnapshotIndex blobStore branchState focusChangeHandler

  liftIO $ print "finish setup"

  flex_p (getBody root) [
      (element branchBrowserRoot, flexGrow 1)
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
