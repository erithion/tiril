{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}

module Windows 
where

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

createMainWindow = UI.div #. "tiril-main-window"

-- The window must exist already
getMainWindow :: UI Element
getMainWindow = head <$> getWindows "tiril-main-window"

clearMainWindow = getMainWindow # set children []

{---------------------------------------------------------}

getWindows :: String -> UI [Element]
getWindows className = flip getElementsByClassName className =<< askWindow

deleteWindows :: String -> UI ()
deleteWindows className = void . sequence =<< map delete <$> getWindows className
          
makeMessageWindow :: String -> String -> String -> UI Element
makeMessageWindow alertType cap msg = do
  clearMainWindow
  win <- UI.div #. "container d-flex align-items-center justify-content-center"
          #+ [ UI.div  
                  #. ("alert " ++ alertType ++ " alert-dismissible fade show")
                  #+ [ UI.h4 #. "alert-heading" # set text cap
                     , UI.a #. "close" # set UI.href "#" # set (attr "data-dismiss") "alert" # set (attr "aria-label") "close" # set text "✖"
                     , UI.hr
                     , UI.p #. "mb-0" # set text msg ] ]
  getMainWindow #+ [ element win ]
  
createMessageGreen :: String -> String -> UI Element
createMessageGreen = makeMessageWindow "alert-success"

createMessageRed :: String -> String -> UI Element
createMessageRed = makeMessageWindow "alert-danger"

createMessageBlue :: String -> String -> UI Element
createMessageBlue = makeMessageWindow "alert-primary"