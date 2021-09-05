{-# LANGUAGE AllowAmbiguousTypes #-}

module HGit.GUI.Elements where

--------------------------------------------
import           Control.Monad.Trans
import           Data.Singletons.TH (SingI, sing)
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
--------------------------------------------
import           HGit.Core.Types
import           HGit.GUI.Core
--------------------------------------------


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


infraDiv :: UI Element
infraDiv = UI.div # withClass ["infra"]

nestedUL :: UI Element
nestedUL = UI.ul # set (attr "style") "margin-left:1em"

nestedDiv :: UI Element
nestedDiv = UI.div # set (attr "style") "margin-left:1em"
