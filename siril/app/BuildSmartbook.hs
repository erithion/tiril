{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module BuildSmartbook where

import           GHC.Generics
import           System.IO
import qualified Data.Text.Lazy                 as T
import qualified Data.Text.Lazy.Encoding        as T
import qualified Data.Text                      as TS
import qualified Data.ByteString                as BS
import           Data.Aeson                     as JS
import qualified Graphics.UI.TinyFileDialogs    as Dlg
import qualified Graphics.UI.Threepenny         as UI
import           Graphics.UI.Threepenny.Core                                    hiding (get)
import qualified Graphics.UI.Threepenny.Core    as TP        (get)
import           GHC.IO.Encoding                             (utf8, setLocaleEncoding)
import           Control.Monad.Trans.Either
import           System.FilePath                            (replaceExtension)

import           Type
import           Windows
import           SmartBook.Sb
import           SmartBook.Crypto
import           SmartBook.Alloy

data JSAlloy = JSAlloy
    { jsEncryptResult :: Bool
    , jsBook :: AlloyBook
    } deriving (Generic, Show)

instance FromJSON JSAlloy

buildBook = do
  clearMainWindow
  -- Modals behave better if suffer little influence from surrounding elements
  -- https://stackoverflow.com/questions/27411179/bootstrap-3-modal-not-working-correctly-when-placed-inside-a-fixed-parent
  bodyElement
      #+ [ createModalWindow "smartbook_modal" "SmartBook" "" ]

  getMainWindow #+ [ 
                     UI.link 
                      # set UI.rel "stylesheet"
                      # set UI.href "/static/book.css"
                   , mkElement "script" # set UI.src "/static/jquery.validate.min.js"
                   , mkElement "script" # set UI.src "/static/syncscroll.js"
                   , UI.div #. "my-msg-container" #+ 
                      [ dismissableAlert 
                        "alert-warning"
                        "Please be advised that it is unofficial!"
                        "Books created this way might stop working any moment \
                        \with any upcoming update of the SmartBook application. Also \
                        \the feature cannot serve as an excuse to avoid paying for the SmartBook application itself. \
                        \It is intended to create the books for personal use only and/or as a last resort \
                        \if you haven't found a book you were looking for among the ones on the official site."
                      ]
                   , metaForm
                   , editors
                   , mkElement "script" # set UI.src "/static/book.js"
                   ]
  where jsGetData :: UI JS.Value
        jsGetData = callFunction $ ffi "getData()"

        bodyElement :: UI Element
        bodyElement = return . head =<< flip getElementsByTagName "body" =<< askWindow
        
        -- monad transformer with failure possibility
        buildBookT :: Result JSAlloy -> TS.Text -> EitherT String IO ()
        buildBookT jsdata file = do
            (dat::JSAlloy) <- case jsdata of
                    Success s -> EitherT . return . Right $ s -- equivalent to return s
                    Error e -> EitherT . return . Left $ e
            sb <- hoistEither $ sbBinary (T.fromStrict file) (jsEncryptResult dat) (jsBook dat)
            let path = flip replaceExtension ".sb" . T.unpack . T.fromStrict $ file
            liftIO $ do
                setLocaleEncoding utf8
                withBinaryFile path WriteMode $ \handle ->
                             BS.hPutStr handle sb
  
        metaForm = do  
            btn <- UI.button #. "my-meta btn my-btn-info btn-lg align-top"
                        # set UI.type_ "button"
                        # set text "Save"
                        # set (attr "disabled") ""
                        
            on UI.click btn $ const $ do
                filename <- liftIO $ Dlg.saveFileDialog "Save SmartBook" "" ["*.sb"] "SmartBook"
                case filename of
                    Just file -> do
                        (jsdata :: Result JSAlloy) <- fromJSON <$> jsGetData
                        res <- liftIO . runEitherT $ buildBookT jsdata file
                        either (showModalWindow "smartbook_modal" . (++) "Error: ") 
                               (const $ showModalWindow "smartbook_modal" "The book has been created successfully!" ) res
                    _ -> return ()
                        
            UI.form #. "my-meta-container"
                # set UI.id_ "ccSelectForm"
                #+  [ UI.input #. "form-control my-meta w-25 p-1"
                        # set UI.id_ "title"
                        # set UI.type_ "text"
                        # set (attr "name") "title"
                        # set (attr "placeholder") "Title"
                        # set (attr "aria-label") "Book title"
                        # set (attr "aria-describedby") "basic-addon1"
                    , UI.input #. "form-control my-meta w-25 p-1"
                        # set UI.id_ "author"
                        # set UI.type_ "text"
                        # set (attr "name") "author"
                        # set (attr "placeholder") "Author"
                        # set (attr "aria-label") "Author"
                        # set (attr "aria-describedby") "basic-addon1"
                    , element btn
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