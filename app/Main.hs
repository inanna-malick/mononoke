{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import           Merkle.Types.BlakeHash
import           Data.Functor.Compose
import           Data.List (intersperse)
import           Data.List.NonEmpty (toList)
import           HGit.Core.Types
import           Data.Singletons.TH (SingI)
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

data State
  = State
  { main_branch :: Hash 'CommitT
  , other_branches:: Map String (Hash 'CommitT)
  }

data ST x = T String x x | N
  deriving (Functor, Foldable, Traversable)

type Expansions = Set RawBlakeHash


ex :: Fix ST
ex = Fix $ T "root" (Fix $ T "c1" (Fix N) (Fix N)) (Fix N)

-- draw the provided tree on the provided UI element
renderST :: Fix ST -> Element -> UI Element
renderST = cata f
  where
    nestedDiv = UI.div #set (attr "style") "margin-left:1em"
    f :: Algebra ST (Element -> UI Element)
    f (T s l r) parent = do
      left <-  nestedDiv >>= l
      right <- nestedDiv >>= r
      str <- UI.div #+ [string s]
      element parent #+ fmap element [str, left, right]
    f N parent = do
      x <- UI.div #+ [string "null"]
      element parent #+ fmap element [x]



uiMT
  :: forall (x :: MTag). SingI x
  => TVar Expansions
  -> LMMT UI x
  -> Element
  -> UI Element
uiMT expansions root parentElement = (getConst $ hcata g root) parentElement
  where
    nestedDiv = UI.div # set (attr "style") "margin-left:1em"
    ul = UI.ul # set (attr "style") "margin-left:1em"
    g :: Alg (LMM UI) (Const (Element -> UI Element))
    g (HC (Tagged (Const raw) (HC (Compose m)))) = Const $ \pn -> do
      let minimized = do
            expand <- UI.button #+ [string "expand"]
            on UI.click expand $ \() -> do
              liftIO $ atomically $ modifyTVar expansions (Set.insert raw)
              _ <- set UI.children [] (element pn)
              expanded

            element pn #+ [element expand]
          expanded = do
            mm <- m
            minimize <- UI.button #+ [string "[-]"]
            on UI.click minimize $ \() -> do
              liftIO $ atomically $ modifyTVar expansions (Set.delete raw)
              _ <- set UI.children [] (element pn)
              minimized

            _ <- element pn #+ [element minimize]
            (getConst $ f mm) pn

      isExpanded <- liftIO $ atomically $ Set.member raw <$> readTVar expansions
      case isExpanded of
        False -> minimized
        True -> expanded
    f :: Alg M (Const (Element -> UI Element))
    f (Snapshot t o ps) = Const $ \pn -> do
        hdr     <- UI.string "Snapshot:"
        tree    <- nestedDiv >>= getConst t
        orig    <- nestedDiv >>= getConst o
        parentList <- ul #+ ((UI.li >>= ) . getConst <$> ps)
        parents <- nestedDiv #+ [string "parents:", element parentList]
        element pn #+ fmap element [hdr, tree, orig, parents]

    f (File b l p)
      = Const $ \pn -> do
        hdr <- UI.string "File:"
        blob    <- nestedDiv >>= getConst b
        lastMod <- nestedDiv >>= getConst l
        prev    <- ul #+ ((UI.li >>= ) . getConst <$> p)
        element pn #+ fmap element [hdr, blob, lastMod, prev]

    f (Dir cs) = Const $ \pn -> do
        let renderChild (k, Const v) = UI.li #+ [string (k ++ ":"), nestedDiv >>= v]
        hdr      <- UI.string "Dir:"
        childrenList <- ul #+ (renderChild <$> Map.toList cs)
        element pn #+ fmap element [hdr, childrenList]


    f NullCommit = Const $ \pn -> element pn #+ [string "NullCommit"]

    f (Commit m cs ps) = Const $ \pn -> do
        hdr <- UI.string "Commit:"
        msg <- UI.string $ "msg: " ++ m

        let renderPath = mconcat . intersperse "/" . toList
            renderChange Change{..} = case _change of
              Del -> UI.li #+ [string $ renderPath _path ++ ": Del"]
              Add (Const blob) ->
                UI.li #+ [UI.string (renderPath _path ++ ": Add: "), nestedDiv >>= blob]


        changeList <- ul #+ (renderChange <$> cs)
        changes <- nestedDiv #+ [string "changes:", element changeList]

        parentList <- ul #+ ((UI.li >>= ) . getConst <$> toList ps)
        parents <- nestedDiv #+ [string "parents:", element parentList]

        element pn #+ fmap element [hdr, msg, changes, parents]

    f (Blob c) = Const $ \pn -> do
        hdr <- UI.string "Blob:"
        content <- UI.string c
        element pn #+ fmap element [hdr, content]




type Database = Map UserName ToDoList
type UserName = String
type ToDoList = Set String

main :: IO ()
main = do
  expansions <- atomically $ newTVar (Set.empty)
  startGUI defaultConfig (setup expansions)

setup :: TVar Expansions -> Window -> UI ()
setup expansions rootWindow = void $ do
  let hashed = liftLMMT commit3
  getBody rootWindow >>= uiMT expansions hashed

  -- userNameInput <- UI.input # set (attr "placeholder") "User name"
  -- loginButton <- UI.button #+ [ string "Login" ]
  -- getBody rootWindow #+
  --   map element [ userNameInput, loginButton ]

  -- on UI.click loginButton $ \_ -> do
  --   userName <- get value userNameInput

  --   currentItems <- fmap Set.toList $ liftIO $ atomically $ do
  --     db <- readTVar database
  --     case Map.lookup userName db of
  --       Nothing -> do
  --          writeTVar database (Map.insert userName Set.empty db)
  --          return Set.empty

  --       Just items -> return items

  --   let showItem item = UI.li #+ [ string item ]
  --   toDoContainer <- UI.ul #+ map showItem currentItems

  --   newItem <- UI.input

  --   on UI.sendValue newItem $ \input -> do
  --     liftIO $ atomically $ modifyTVar database $
  --       Map.adjust (Set.insert input) userName

  --     set UI.value "" (element newItem)
  --     element toDoContainer #+ [ showItem input ]

  --   header <- UI.h1 #+ [ string $ userName ++ "'s To-Do List" ]
  --   set children
  --     [ header, toDoContainer, newItem ]
  --     (getBody rootWindow)


-- list of branches / commit creation interface | file snapshot viewer w/ edit capability | read-only merkle structure navigator over commit tree of current parent
-- part 2 is merging - merge based on branch A + B + C, merge conflicts that must be resolved marked using special GUI element in middle panel



-- concept: multiple columns showing different info
-- leftmost: list of branches, each given a name
-- each branch has UI via buttons:
-- - makeCommit - create a new WIP commit in middle column based on this
-- - branch (create new branch == to head commit)
-- - delete
-- middle column shows current commit w/ lazy expansion
-- - can add parents to NEL
-- - can edit commit message
-- - can add changes
-- rightmost column
-- - resulting file tree from currently selected commit
-- - or file tree or file tree with (via either) merge errors at leaves for new commit?
