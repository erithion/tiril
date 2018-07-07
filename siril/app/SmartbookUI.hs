{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module SmartbookUI where

import           System.IO
import           GHC.Generics
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Maybe
import           Data.Maybe
import           Data.Aeson                     as JS
import qualified Data.Text.Lazy                 as T
import qualified Data.Text.Lazy.Encoding        as T
import qualified Data.Text.Lazy.IO              as T
import qualified Data.Text                      as TS
import qualified Data.ByteString                as BS
import qualified Graphics.UI.TinyFileDialogs    as Dlg
import qualified Graphics.UI.Threepenny         as UI
import qualified Graphics.UI.Threepenny.Core    as TP        (get)
import           Graphics.UI.Threepenny.Core                                    hiding (get)
import           GHC.IO.Encoding                             (utf8, setLocaleEncoding)
import           System.FilePath                             (replaceExtension)
import           Control.Monad                               (when)
import           Data.Either.Extra                           (isRight, fromRight')

import           Type
import           Windows
import           SmartBook.Type
import           SmartBook.Crypto
import           SmartBook.Alloy

data JSAlloy = JSAlloy
    { jsEncryptResult :: Bool
    , jsBook :: AlloyBook
    } deriving (Generic, Show)

instance FromJSON JSAlloy

buildBook = do
  clearMainWindow
  -- Modal might not play nicely unless it is a direct child of <body>
  -- https://stackoverflow.com/questions/27411179/bootstrap-3-modal-not-working-correctly-when-placed-inside-a-fixed-parent
  getBodyElement
      #+ [ modalBtnClose "smartbookStatusModal" "SmartBook" Nothing
         , modalBtnOkCancel "smartbookEncryptModal" "SmartBook" . Just $
                "The book will be saved in plain JSON, \
                \so you won't be able to use it in the SmartBook application \
                \unless you encrypt it afterwards. \
                \Would you like to uncheck the option anyway?" ]

  getMainWindow #+ 
    [ UI.link
        # set UI.rel "stylesheet"
        # set UI.href "/static/book.css"
    , mkElement "script" # set UI.src "/static/jquery.validate.min.js"
    , mkElement "script" # set UI.src "/static/syncscroll.js"
    , UI.div #. "my-msg-container" #+ 
        [ dismissableAlert "alert-warning" "Please be advised that it is unofficial!"
            "Books created this way might stop working any moment \
            \with any upcoming update of the SmartBook application. Also \
            \the feature cannot serve as an excuse to avoid paying for the SmartBook application itself. \
            \It is intended to create books for personal use only and/or as a last resort \
            \if you haven't found a book you were looking for among the ones on the official site."
        ]
    , metaForm
    , editors
    , mkElement "script" # set UI.src "/static/book.js" ]
  where jsGetData :: UI JS.Value
        jsGetData = callFunction $ ffi "getData()"

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
                        # set UI.id_ "save"
                        # set text "Save"
                        # set (attr "disabled") ""
                        
            on UI.click btn $ const $ do
                filename <- liftIO $ Dlg.saveFileDialog "Save SmartBook" "" ["*.sb"] "SmartBook"
                when (isJust filename) $ do
                    (jsdata :: Result JSAlloy) <- fromJSON <$> jsGetData
                    res <- liftIO . runEitherT $ buildBookT jsdata (fromJust filename)
                    either (showModalWindow "smartbookStatusModal" . Just . (++) "Error: ") 
                           (const $ showModalWindow "smartbookStatusModal" . Just $ "The book was created successfully!" ) res
                        
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
                    , UI.div #. "col-auto my-1"
                        #+ [ UI.div #. "custom-control custom-checkbox mr-sm-2"
                                #+ [ UI.input #. "custom-control-input"
                                        # set UI.type_ "checkbox"
                                        # set UI.id_ "encryptId"
                                        # set (attr "checked") "checked"
                                   , UI.label #. "custom-control-label"
                                        # set (attr "for") "encryptId"
                                        # set UI.text "Encrypt"
                                   ]
                           ]
                    , element btn
                    ]
                    
        editors = 
            UI.div #. "my-edit-container"
                #+  [ UI.div #. "form-control my-edit syncscroll"
                        # set UI.id_ "leftEditor"
                        # set (attr "name") "myElements"
                        # set (attr "data-placeholder") "Book"
                        # set (attr "aria-label") "With textarea"
                        # set (attr "title") "Load your book"
                    , UI.div #. "form-control my-edit syncscroll"
                        # set UI.id_ "rightEditor"
                        # set (attr "name") "myElements"
                        # set (attr "data-placeholder") "Translation"
                        # set (attr "aria-label") "With textarea"
                        # set (attr "title") "Load your translation"
                    ]   
                    
encryptBook = do
  getBodyElement
      #+ [ modalBtnClose "smartbookEncryptStatusModal" "SmartBook - Encrypt" Nothing]                    
  result <- liftIO $ do
    res <- runMaybeT filesT
    case res of 
        Just (fileIn, fileOut)  -> do
            let (fileIn, fileOut) = fromJust res
            setLocaleEncoding utf8
            input <- T.readFile . T.unpack . T.fromStrict $ fileIn
            let enc = encrypt input
            let path = flip replaceExtension ".sb" . T.unpack . T.fromStrict $ fileOut
            withBinaryFile path WriteMode $ \handle ->
                                 BS.hPutStr handle enc
            return . Just $ "Operation completed successfully!"
        _ -> return Nothing
  when (isJust result) $ do
      showModalWindow "smartbookEncryptStatusModal" result
  where filesT :: MaybeT IO (TS.Text, TS.Text)
        filesT = do
            (fileIn::[TS.Text]) <- MaybeT . liftIO $ Dlg.openFileDialog "Encryption - choose a file to encrypt" "" ["*.*"] "Any file" False
            (fileOut::TS.Text) <- MaybeT . liftIO $ Dlg.saveFileDialog "Encryption - choose a file to save to" "" ["*.sb"] "SmartBook"
            return (head fileIn, fileOut)

decryptBook = do
  getBodyElement
      #+ [ modalBtnClose "smartbookDecryptStatusModal" "SmartBook - Decrypt" Nothing]                    
  result <- liftIO $ do
    res <- runMaybeT filesT
    case res of 
        Just (fileIn, fileOut)  -> do
            let (fileIn, fileOut) = fromJust res
            setLocaleEncoding utf8
            input <- T.readFile . T.unpack . T.fromStrict $ fileIn
            let path = T.unpack . T.fromStrict $ fileOut
            let enc = decrypt input
            when (isRight enc) $ do
                withBinaryFile path WriteMode $ \handle ->
                                 -- fromRight' is partial, so don't remove the guard
                                 BS.hPutStr handle (fromRight' enc)
            return $ either ( Just . (++) "Error: " ) 
                            ( const $ Just "Operation completed successfully!" ) enc
        _ -> return Nothing
  when (isJust result) $ do
      showModalWindow "smartbookDecryptStatusModal" result
  where filesT :: MaybeT IO (TS.Text, TS.Text)
        filesT = do
            (fileIn::[TS.Text]) <- MaybeT . liftIO $ Dlg.openFileDialog "Decryption - choose a file to decrypt" "" ["*.sb"] "SmartBook" False
            (fileOut::TS.Text) <- MaybeT . liftIO $ Dlg.saveFileDialog "Decryption - chose a file to save to" "" ["*.*"] "Any file"
            return (head fileIn, fileOut)
            