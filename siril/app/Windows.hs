{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}

module Windows 
where

import           Data.Maybe
import qualified Data.Text.Lazy                 as T
import qualified Data.ByteString.UTF8           as BU
import qualified Text.Show.Unicode              as US 
import           Blaze.ByteString.Builder                   (copyByteString)
import           Control.Monad                                                  hiding (when)
import qualified Control.Monad                  as M        (when)
import           Data.String                                (IsString)
import qualified Graphics.UI.Threepenny         as UI
import           Graphics.UI.Threepenny.Core                                    hiding (get)
import qualified Graphics.UI.Threepenny.Core    as TP        (get)
import           Data.Tuple.Extra                            ((***), (&&&))
import           Data.Either
import           Control.Concurrent.Async                                       hiding (link)

getBodyElement :: UI Element
getBodyElement = return . head =<< flip getElementsByTagName "body" =<< askWindow

createMainWindow = UI.div #. "tiril-main-window"

-- The window must exist already
getMainWindow :: UI Element
getMainWindow = head <$> getWindows "tiril-main-window"

clearMainWindow = do 
    getMainWindow # set children []

{---------------------------------------------------------}

getWindows :: String -> UI [Element]
getWindows className = flip getElementsByClassName className =<< askWindow

deleteWindows :: String -> UI ()
deleteWindows className = void . sequence =<< map delete <$> getWindows className
          
dismissableAlert :: String -> String -> String -> UI Element
dismissableAlert alertType strongText msg = UI.div  
                  #. ("alert " ++ alertType ++ " alert-dismissible fade show")
                  # set (attr "role") "alert"
                  #+ [ UI.strong # set text (strongText ++ " ")
                     , mkElement "regular" # set text msg
                     , UI.button #. "close" # set UI.type_ "button" # set (attr "data-dismiss") "alert" # set (attr "aria-label") "close" 
                        #+ [ UI.span # set (attr "aria-hidden") "true" # set text "✖" ]
                     ]
                     
showModalWindow :: String -> Maybe String -> UI ()           
showModalWindow modId Nothing = runFunction $ ffi "$(%1).modal('show')" ("#" ++ modId)
showModalWindow modId (Just msg) = runFunction $ ffi "$(%2).text(%3); $(%1).modal('show')" ("#" ++ modId) ("#" ++ modId ++ " .modal-body") msg

-- Modal window constructors
modalBtnClose = modalWindowTemplate [("close", "Close")]
modalBtnOkCancel = modalWindowTemplate [("cancel", "Cancel"), ("ok", "Ok")]

type ButtonId = String
type ButtonName = String
type ModalId = String

modalWindowTemplate :: [(ButtonId, ButtonName)] -> ModalId -> String -> Maybe String -> UI Element
modalWindowTemplate buttons modId cap msg = do
    -- deleting previous if exists
    deleteDOMElements $ "#" ++ modId
    UI.div #. "modal fade" 
        # set UI.id_ modId
        # set (attr "tabindex") "-1"
        # set (attr "role") "dialog"
        # set (attr "aria-labelledby") "exampleModalLabel" -- ???
        # set (attr "aria-hidden") "true"
        #+ [UI.div #. "modal-dialog modal-dialog-centered" # set (attr "role") "document"
                #+ [UI.div #. "modal-content"
                        #+ [ UI.div #. "modal-header"
                                #+ [ UI.h5 #. "modal-title" # set text cap
                                   , UI.button #. "close" 
                                        # set UI.type_ "button" 
                                        # set (attr "data-dismiss") "modal"
                                        # set (attr "aria-label") "Close"
                                        #+ [UI.span # set (attr "aria-hidden") "true" # set text "×"]
                                   ]
                           , UI.div #. "modal-body" # set text (fromMaybe "" msg)
                           , UI.div #. "modal-footer" 
                                #+ (createButton <$> buttons)
                           ]
                   ]
           ]    
    where createButton (btnId, btnName) = 
            UI.button #. "btn btn-secondary" 
                      # set UI.id_ btnId
                      # set UI.type_ "button" 
                      # set (attr "data-dismiss") "modal"
                      # set text btnName
          -- threepenny crashes upon searching for non-existing element
          deleteDOMElements :: String -> UI ()
          deleteDOMElements selector = callFunction (ffi "$(%1).remove()" selector)          