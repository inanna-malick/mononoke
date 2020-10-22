{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import           Merkle.Types.BlakeHash
import           Data.Functor.Compose
import           Data.List (intersperse)
import           Data.List.NonEmpty (toList, nonEmpty, (<|), NonEmpty(..))
import           Data.Singletons.TH (SingI, sing)
--------------------------------------------
import           HGit.Core.Types
import           HGit.Core.MergeTrie
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


-- data State m
--   = State
--   {
--   -- scratchpad that allows for anything from unchanged snapshots [1..n]-way merges and changes
--     workspace :: Fix (STM `Compose` MergeTrie m)
--   -- , browser_state :: Browser m
--   }

data CommitFocus = MainBranch | OtherCommit String

type Expansions = Set RawBlakeHash

data Focus m
  = SnapshotF (WIPT m 'SnapshotT)
  | FileTreeF (WIPT m 'FileTree)
  | CommitF   (WIPT m 'CommitT)
  | BlobF     (WIPT m 'BlobT)


-- (sing :: Sing i)
wrapFocus :: forall (i :: MTag) m. Sing i -> WIPT m i -> Focus m
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

      focus  <- UI.button #+ [string "[commit]"]
      on UI.click focus $ \() -> do
        -- TODO: get from tvar - commit may change w/o regening this element? idk mb
        liftIO $ focusChangeHandler (CommitF $ unmodifiedWIP commit)


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

      snap <- updateSnapshotIndexLMMT store commitSnapshotIndex commit
      focusSnap <- UI.button #+ [string "[snap]"]
      on UI.click focusSnap $ \() -> do
        liftIO $ focusChangeHandler (SnapshotF $ unmodifiedWIP snap)

      _ <- element thisBranch #+ [element focusSnap]


      element pn #+ [element thisBranch]



data UpdateMergeTrie m
  = ApplyChange (Change (WIPT m))
  | AddParent (LMMT m 'CommitT)
  | ResetWithParent (LMMT m 'CommitT)




browseMergeTrie
  :: Handler (UpdateMergeTrie UI)
  -> Handler (Focus UI)
  -> TVar Expansions
  -> Fix (MergeTrie UI)
  -> Element
  -> UI Element
browseMergeTrie modifyMergeTrieHandler focusHandler expansions root parentElement = (cata f root) [] parentElement
  where
    f :: MergeTrie UI ([Path] -> Element -> UI Element) -> [Path] -> Element -> UI Element
    f mt path pn = case mtChange mt of
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

              delChange <- UI.button
              on UI.click delChange $ \input -> do
                liftIO $ modifyMergeTrieHandler $ ApplyChange $ del nepath

              element pn #+ fmap element [addChange, delChange, files, children]

    renderFiles fs = do
      pn <- UI.ul
      traverse (renderFile pn) fs
      element pn

    renderFile pn _ = element pn #+ [string "todo: render file"]

    -- TODO: allow for focus on WIPT trees? mb?
    renderChange :: ChangeType (WIPT UI) -> UI Element
    renderChange (Add wipt) = do
      x <- UI.div
      (HC (Tagged _ blob)) <- fetchWIPT wipt

      focus <- UI.button #+ [string "[<>]"]
      on UI.click focus $ \() -> do
        -- FIXME: don't want to allow focus for WIPT, I think - have this be conditional
        liftIO $ focusHandler $ BlobF wipt

      let shim :: forall w. M w 'BlobT -> String
          shim (Blob blobstr) = blobstr
      let blobstr = shim blob

      element x #+ [string "ADD", element focus, string blobstr]
    renderChange Del = string "DELETE"


    renderChildren path c = do
      pn <- UI.ul
      traverse (renderChild path pn) $ Map.toList c
      element pn

    renderChild :: [Path] -> Element -> (Path, WIPT UI 'FileTree `Either` ([Path] -> Element -> UI Element)) -> UI Element
    renderChild path pn (pathSegment, Left wipt) = element pn #+ [UI.li >>= browseWIPT focusHandler expansions wipt]
    renderChild path pn (pathSegment, Right next) = element pn #+ [UI.li >>= next (path ++ [pathSegment])]



browseWIPT
  :: forall (x:: MTag). SingI x
  => Handler (Focus UI)
  -> TVar Expansions
  -> WIPT UI x
  -> Element
  -> UI Element
browseWIPT focusHandler expansions root parentElement
  = (getConst $ hpara (uiWIPAlg focusHandler expansions) root) parentElement


uiWIPAlg
  :: Handler (Focus UI)
  -> TVar Expansions
  -> RAlg (WIP UI) (Const (Element -> UI Element))
uiWIPAlg focusHandler expansions (HC (L lmmt)) = Const $ \pn -> browseLMMT focusHandler expansions (wrapFocus sing $ unmodifiedWIP lmmt) pn
uiWIPAlg focusHandler expansions (HC (R (HC (Tagged h m)))) = Const $ \pn -> -- TODO: proper WIPT UI, this is a hack
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
    SnapshotF root -> (getConst $ hpara (uiWIPAlg focusHandler expansions) root) parentElement
    FileTreeF root -> (getConst $ hpara (uiWIPAlg focusHandler expansions) root) parentElement
    CommitF   root -> (getConst $ hpara (uiWIPAlg focusHandler expansions) root) parentElement
    BlobF     root -> (getConst $ hpara (uiWIPAlg focusHandler expansions) root) parentElement


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
          liftIO $ focusHandler $ wrapFocus sing $ unmodifiedWIP (Term $ hfmap _tag x)
        _ <- element pn #+ [element focus]
        pure ()

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



setup :: Window -> UI ()
setup root = void $ do
  let initialBranchState = BranchState
                         { bsMainBranch = liftLMMT commit3
                         , bsBranches = []
                         }


  commitSnapshotIndexTVar <- liftIO . atomically $ newTVar Map.empty
  let commitSnapshotIndex = stmIOIndex commitSnapshotIndexTVar

  inProgressCommitTVar :: TVar (M (WIPT UI) 'CommitT)
    <- liftIO . atomically $ newTVar $ Commit "" [] (unmodifiedWIP (bsMainBranch initialBranchState) :| [])

  blobStoreTvar <- liftIO . atomically $ newTVar emptyBlobStore
  let blobStore = stmIOStore blobStoreTvar

  initCommitHash <- uploadM (sWrite blobStore) commit3
  let innitCommit = expandHash (sRead blobStore) initCommitHash

  -- mergeState <- liftIO . atomically $ newTVar mt
  branchState <- liftIO . atomically $ newTVar initialBranchState
  expansions <- liftIO . atomically $ newTVar (Set.empty)

  (focusChangeEvent, focusChangeHandler) <- liftIO $ newEvent
  (modifyMergeTrieEvent, modifyMergeTrieHandler) <- liftIO $ newEvent


  mergeTrieRoot <- simpleDiv

  let extractFT :: M x 'SnapshotT -> x 'FileTree
      extractFT (Snapshot ft _ _) = ft
  let handleMMTE msg = do
        _ <- element mergeTrieRoot # set children []
        commit <- liftIO $ atomically $ handleMMTE' msg
        snap <- makeSnapshot commit (iRead commitSnapshotIndex) (sRead blobStore)
        mt <- buildMergeTrie emptyMergeTrie (extractFT snap)
        _ <- browseMergeTrie modifyMergeTrieHandler focusChangeHandler expansions mt mergeTrieRoot
        pure ()

      handleMMTE' :: UpdateMergeTrie UI -> STM (M (WIPT UI) 'CommitT)
      handleMMTE' (ApplyChange change) = do
          c <- readTVar inProgressCommitTVar
          let nextCommit = case c of
                (Commit s cs ps) -> Commit s (cs ++ [change]) ps
                -- SHOULD NEVER HAPPEN... FIXME
                NullCommit -> Commit "" [change] (modifiedWIP NullCommit :| [])
          writeTVar inProgressCommitTVar nextCommit
          pure nextCommit

      handleMMTE' (AddParent parentToAdd) = do
          c <- readTVar inProgressCommitTVar
          let nextCommit = case c of
                (Commit s cs ps) -> Commit s cs (unmodifiedWIP parentToAdd <| ps)
                -- SHOULD NEVER HAPPEN... FIXME
                NullCommit -> Commit "" [] (unmodifiedWIP parentToAdd :| [modifiedWIP NullCommit])
          writeTVar inProgressCommitTVar nextCommit
          pure nextCommit

      handleMMTE' (ResetWithParent parent) = do
          let nextCommit = Commit "todo: figure out msg interface" [] (unmodifiedWIP parent :| [])
          writeTVar inProgressCommitTVar nextCommit
          pure nextCommit


  -- discarded return value deregisters handler
  _ <- onEvent modifyMergeTrieEvent handleMMTE


  liftIO $ modifyMergeTrieHandler $ ResetWithParent $ bsMainBranch initialBranchState


  browserRoot <- simpleDiv
  -- discarded return value deregisters handler
  _ <- onEvent focusChangeEvent $ \focus -> do
    _ <- element browserRoot # set children []
    browseLMMT focusChangeHandler expansions focus browserRoot
    pure ()

  liftIO $ focusChangeHandler $ wrapFocus sing $ unmodifiedWIP innitCommit

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
