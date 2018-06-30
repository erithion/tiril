{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BuildSmartbook where

import qualified Data.Text.Lazy                 as T
import qualified Data.Text                      as TS
import           Control.Monad                                                  hiding (when)
import qualified Control.Monad                  as M        (when)
import qualified Graphics.UI.Threepenny         as UI
import           Graphics.UI.Threepenny.Core                                    hiding (get)
import qualified Graphics.UI.Threepenny.Core    as TP        (get)
import           Control.Concurrent                          (forkIO)
import           Data.Tuple.Extra                            ((***), (&&&))
import           Data.Either
import           Control.Concurrent.Async                                       hiding (link)
import           Data.Hashable                               (hash)

import           Type
import           Windows

buildBook = do
  clearMainWindow
  getMainWindow #+ [ 
                     UI.link 
                      # set UI.rel "stylesheet"
                      # set UI.href "/static/book.css"
                   , mkElement "script" # set UI.src "/static/jquery.validate.min.js"
                   , mkElement "script" # set UI.src "/static/syncscroll.js"
                   , metaForm
                   , editors
                   , mkElement "script" # set UI.src "/static/book.js"
                   ]
  where metaForm = 
            UI.form #. "my-meta-container"
                # set UI.id_ "ccSelectForm"
                #+  [ UI.input #. "form-control my-meta w-25 p-1"
                        # set UI.type_ "text"
                        # set (attr "name") "title"
                        # set (attr "placeholder") "Title"
                        # set (attr "aria-label") "Book title"
                        # set (attr "aria-describedby") "basic-addon1"
                    , UI.input #. "form-control my-meta w-25 p-1"
                        # set UI.type_ "text"
                        # set (attr "name") "author"
                        # set (attr "placeholder") "Author"
                        # set (attr "aria-label") "Author"
                        # set (attr "aria-describedby") "basic-addon1"
                    , UI.button #. "my-meta btn my-btn-info btn-lg align-top"
                        # set UI.type_ "button"
                        # set text "Save"
                        # set (attr "disabled") ""
                    ]
        editors = 
            UI.div #. "my-edit-container"
                #+  [ UI.div #. "form-control my-edit syncscroll"
                        # set UI.id_ "editor"
                        # set (attr "name") "myElements"
                        # set (attr "data-placeholder") "A book in your mother tongue"
                        # set (attr "aria-label") "With textarea"
                    , UI.div #. "form-control my-edit syncscroll"
                        # set UI.id_ "editor2"
                        # set (attr "name") "myElements"
                        # set (attr "data-placeholder") "A book in a foreign language"
                        # set (attr "aria-label") "With textarea"
                    ]   