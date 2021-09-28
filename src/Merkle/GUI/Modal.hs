{-# LANGUAGE RecordWildCards #-}

module Merkle.GUI.Modal where

--------------------------------------------
import           Control.Monad (void)
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
--------------------------------------------
import           Merkle.GUI.Elements (withClass)
--------------------------------------------

data SpawnPopup m
  = SpawnError String
  | SpawnRequestText
      String -- window name
      (String -> m ()) -- action to run given input (always string via text... could be better there)


handleSpawnPopup :: Element -> SpawnPopup UI -> UI ()
handleSpawnPopup modalRoot (SpawnError s) = void $ do
    _ <- element modalRoot # set children []
    element modalRoot #+ [ drawModal "error!" [string s] ]

handleSpawnPopup modalRoot (SpawnRequestText s act) = void $ do
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
