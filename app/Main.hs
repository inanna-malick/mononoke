{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import           Merkle.Types.BlakeHash
import           Data.Functor.Compose
import           Data.List (intersperse)
import           Data.List.NonEmpty (toList)
import           HGit.Core.Types
import           HGit.Core.MergeTrie
import           Data.Singletons.TH (SingI, sing)
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


data State m
  = State
  {
  -- scratchpad that allows for anything from unchanged snapshots [1..n]-way merges and changes
    workspace :: Fix (MergeTrie m)
  -- , browser_state :: Browser m
  }

data CommitFocus = MainBranch | OtherCommit String

type Expansions = Set RawBlakeHash

data Focus m
  = SnapshotF (LMMT m 'SnapshotT)
  | FileTreeF (LMMT m 'FileTree)
  | CommitF   (LMMT m 'CommitT)
  | BlobF     (LMMT m 'BlobT)


-- (sing :: Sing i)
wrapFocus :: forall (i :: MTag) m. Sing i -> LMMT m i -> Focus m
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
  -> Handler (Focus UI)
  -> UI Element
branchBrowser parentElement commitSnapshotIndex store branchState focusChangeHandler = do
    bs <- liftIO $ atomically $ readTVar branchState
    branchList <- UI.ul
    drawBranch False branchList ("main", bsMainBranch bs)
    _ <- traverse (drawBranch True branchList) (bsBranches bs)

    addBranch <- UI.input
    on UI.sendValue addBranch $ \input -> do
      let branch = (input, liftLMMT $ Term $ NullCommit)
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

      focus  <- UI.button #+ [string "[commit]"]
      on UI.click focus $ \() -> do
        liftIO $ focusChangeHandler (CommitF commit)


      _ <- element thisBranch #+ [string branchName, element focus]

      if not del then pure () else do
        deleteButton <- UI.button #+ [string "[-]"]
        on UI.click deleteButton $ \() -> do
          -- remove via branch name, simple/easy
          liftIO $ atomically $ modifyTVar branchState $ \bs ->
            bs { bsBranches = filter (not . (== branchName) . fst) $ bsBranches bs }
          delete thisBranch
        _ <- element thisBranch #+ [element deleteButton]
        pure ()

      snap <- updateSnapshotIndex store commitSnapshotIndex commit
      focusSnap <- UI.button #+ [string "[snap]"]
      on UI.click focusSnap $ \() -> do
        liftIO $ focusChangeHandler (SnapshotF snap)

      _ <- element thisBranch #+ [element focusSnap]


      element pn #+ [element thisBranch]





browseMergeTrie
  :: Handler (Focus UI)
  -> Fix (MergeTrie UI)
  -> Element
  -> UI Element
browseMergeTrie focusHandler root parentElement = (cata f root) parentElement
  where
    f :: MergeTrie UI (Element -> UI Element) -> Element -> UI Element
    f mt pn = case mtChange mt of
      Just c -> do
        children <- renderChildren $ mtChildren mt
        files <- renderFiles $ mtFilesAtPath mt
        change <- UI.div #+ [string "change: ", renderChange c]
        element pn #+ fmap element [change, files, children]
      Nothing -> do
        children <- renderChildren $ mtChildren mt
        files <- renderFiles $ mtFilesAtPath mt
        element pn #+ fmap element [files, children]

    renderFiles fs = do
      pn <- UI.ul
      traverse (renderFile pn) fs
      element pn

    renderFile pn _ = element pn #+ [string "todo: render file"]

    renderChange (Add blob@(Term(HC (Tagged _ (HC (Compose m)))))) = do
      x <- UI.div

      focus <- UI.button #+ [string "[<>]"]
      on UI.click focus $ \() -> do
        liftIO $ focusHandler $ BlobF blob

      let shim :: forall w. M w 'BlobT -> String
          shim (Blob blobstr) = blobstr
      blobstr <- shim <$> m

      element x #+ [string "ADD", element focus, string blobstr]
    renderChange Del = string "DELETE"


    renderChildren c = do
      pn <- UI.ul
      traverse (renderChild pn) $ Map.toList c
      element pn

    renderChild :: Element -> (Path, WIPT m 'FileTree `Either` (Element -> UI Element)) -> UI Element
    renderChild pn (path, Left wipt) = element pn #+ [string "todo: render WIPT child"]
    renderChild pn (path, Right next) = element pn #+ [UI.li >>= next]



browseWIPT
  :: forall (x:: MTag). SingI x
  => Handler (Focus UI)
  -> TVar Expansions
  -> WIPT UI x
  -> Element
  -> UI Element
browseWIPT focusHandler expansions root parentElement
  = (getConst $ hpara f root) parentElement
  where
    f :: RAlg (WIP UI) (Const (Element -> UI Element))
    f (HC (L lmmt)) = Const $ \pn -> browseLMMT focusHandler expansions (wrapFocus sing lmmt) pn
    f (HC (R (HC (Tagged h m)))) = Const $ \pn -> -- TODO: proper WIPT UI, this is a hack
      -- (getConst $ uiMAlg $ HC $ Tagged h $ HC $ Compose $ pure $ hfmap _elem m) pn
      (getConst $ uiMAlg $ hfmap _elem m) pn
-- type WIP m = HEither (LMMT m) `HCompose` Tagged Hash `HCompose` M


browseLMMT
  :: Handler (Focus UI)
  -> TVar Expansions
  -> Focus UI
  -> Element
  -> UI Element
browseLMMT focusHandler expansions focus parentElement = case focus of
    SnapshotF root -> (getConst $ hpara (uiLMMAlg focusHandler expansions) root) parentElement
    FileTreeF root -> (getConst $ hpara (uiLMMAlg focusHandler expansions) root) parentElement
    CommitF   root -> (getConst $ hpara (uiLMMAlg focusHandler expansions) root) parentElement
    BlobF     root -> (getConst $ hpara (uiLMMAlg focusHandler expansions) root) parentElement


nestedUL :: UI Element
nestedUL = UI.ul # set (attr "style") "margin-left:1em"

nestedDiv :: UI Element
nestedDiv = UI.div # set (attr "style") "margin-left:1em"


uiLMMAlg
  :: Handler (Focus UI)
  -> TVar Expansions
  -> RAlg (LMM UI) (Const (Element -> UI Element))
uiLMMAlg focusHandler expansions x@(HC (Tagged (Const raw) (HC (Compose m)))) = Const $ \pn -> do
  let drawFocus = do
        focus <- UI.button #+ [string "[<>]"]
        on UI.click focus $ \() -> do
          liftIO $ focusHandler $ wrapFocus sing (Term $ hfmap _tag x)
        element pn #+ [element focus]

      minimized = do
        expand <- UI.button #+ [string $ "[+" ++ show raw ++ "]"]
        on UI.click expand $ \() -> do
          liftIO $ atomically $ modifyTVar expansions (Set.insert raw)
          _ <- set UI.children [] (element pn)
          expanded
        drawFocus
        element pn #+ [element expand]

      expanded = do
        mm <- m
        minimize <- UI.button #+ [string "[-]"]
        on UI.click minimize $ \() -> do
          liftIO $ atomically $ modifyTVar expansions (Set.delete raw)
          _ <- set UI.children [] (element pn)
          minimized
        drawFocus
        _ <- element pn #+ [element minimize]
        (getConst $ uiMAlg $ hfmap _elem mm) pn

  isExpanded <- liftIO $ atomically $ Set.member raw <$> readTVar expansions
  case isExpanded of
    False -> minimized
    True -> expanded


uiMAlg :: Alg M (Const (Element -> UI Element))
uiMAlg (Snapshot t o ps) = Const $ \pn -> do
    hdr     <- UI.string "Snapshot:"
    tree    <- nestedDiv >>= getConst t
    orig    <- nestedDiv >>= getConst o
    parentList <- nestedUL #+ ((UI.li >>= ) . getConst <$> ps)
    parents <- nestedDiv #+ [string "parents:", element parentList]
    element pn #+ fmap element [hdr, tree, orig, parents]

uiMAlg (File b l p)
  = Const $ \pn -> do
    hdr <- UI.string "File:"
    blob    <- nestedDiv >>= getConst b
    lastMod <- nestedDiv >>= getConst l
    prev    <- nestedUL #+ ((UI.li >>= ) . getConst <$> p)
    element pn #+ fmap element [hdr, blob, lastMod, prev]

uiMAlg (Dir cs) = Const $ \pn -> do
    let renderChild (k, Const v) = UI.li #+ [string (k ++ ":"), nestedDiv >>= v]
    hdr      <- UI.string "Dir:"
    childrenList <- nestedUL #+ (renderChild <$> Map.toList cs)
    element pn #+ fmap element [hdr, childrenList]


uiMAlg NullCommit = Const $ \pn -> element pn #+ [string "NullCommit"]

uiMAlg (Commit m cs ps) = Const $ \pn -> do
    hdr <- UI.string "Commit:"
    msg <- UI.string $ "msg: " ++ m

    let renderPath = mconcat . intersperse "/" . toList
        renderChange Change{..} = case _change of
          Del -> UI.li #+ [string $ renderPath _path ++ ": Del"]
          Add (Const blob) ->
            UI.li #+ [UI.string (renderPath _path ++ ": Add: "), nestedDiv >>= blob]


    changeList <- nestedUL #+ (renderChange <$> cs)
    changes <- nestedDiv #+ [string "changes:", element changeList]

    parentList <- nestedUL #+ ((UI.li >>= ) . getConst <$> toList ps)
    parents <- nestedDiv #+ [string "parents:", element parentList]

    element pn #+ fmap element [hdr, msg, changes, parents]

uiMAlg (Blob c) = Const $ \pn -> do
    hdr <- UI.string "Blob:"
    content <- UI.string c
    element pn #+ fmap element [hdr, content]




type Database = Map UserName ToDoList
type UserName = String
type ToDoList = Set String

main :: IO ()
main = do
  startGUI defaultConfig setup


-- NOTE/TODO: this could all be in a single STM transaction. wild.
updateSnapshotIndex :: MonadIO m => Store m -> Index m -> LMMT m 'CommitT -> m (LMMT m 'SnapshotT)
updateSnapshotIndex store index commit = do
  msnap <- (iRead index) (hashOfLMMT commit)
  case msnap of
    Just h  -> do
      pure $ expandHash (sRead store) h
    Nothing -> do
      snap <- makeSnapshot commit (iRead index) (sRead store)
      let wipt = modifiedWIP snap
      uploadedSnap <- uploadWIPT (sWrite store) wipt
      (iWrite index) (hashOfLMMT commit) (hashOfLMMT uploadedSnap)
      pure uploadedSnap


setup :: Window -> UI ()
setup root = void $ do
  let initialBranchState = BranchState
                         { bsMainBranch = liftLMMT commit3
                         , bsBranches = []
                         }


  commitSnapshotIndexTVar <- liftIO . atomically $ newTVar Map.empty
  let commitSnapshotIndex = stmIOIndex commitSnapshotIndexTVar


  blobStoreTvar <- liftIO . atomically $ newTVar emptyBlobStore
  let blobStore = stmIOStore blobStoreTvar

  initCommitHash <- uploadM (sWrite blobStore) commit3
  let innitCommit = expandHash (sRead blobStore) initCommitHash


  -- TODO: update all this to use snapshot file tree as basis for merge tree in center panel
  -- TODO: tomorrow! today is done but for the dregs
  -- FIXME: SHIM
  -- mainBranchSnapshot <- updateSnapshotIndex blobStore commitSnapshotIndex $ bsMainBranch initialBranchState
  -- let extractFT :: LMMT (WIPT m) 'SnapshotT -> m ()
  --     extractFT (Snapshot ft _ _) = ft
  -- mt <- buildMergeTrie emptyMergeTrie (extractFT mainBranchSnapshot)

  -- mergeState <- liftIO . atomically $ newTVar mt
  branchState <- liftIO . atomically $ newTVar initialBranchState
  expansions <- liftIO . atomically $ newTVar (Set.empty)

  (focusChangeEvent, focusChangeHandler) <- liftIO $ newEvent

  mergeTrieRoot <- simpleDiv
  -- _ <- browseMergeTrie focusChangeHandler mt mergeTrieRoot

  browserRoot <- simpleDiv
  -- discarded return value deregisters handler
  _ <- onEvent focusChangeEvent $ \focus -> do
    element browserRoot # set children []
    browseLMMT focusChangeHandler expansions focus browserRoot
    pure ()

  liftIO $ focusChangeHandler $ wrapFocus sing innitCommit

  branchBrowserRoot <- simpleDiv
  branchBrowser branchBrowserRoot commitSnapshotIndex blobStore branchState focusChangeHandler

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
