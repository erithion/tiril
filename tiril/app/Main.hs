{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}
module Main where

import qualified Data.Text                      as TS
import qualified Data.Text.Lazy                 as T
import qualified Data.ByteString.UTF8           as BU
import qualified Text.Show.Unicode              as US 
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.HTTP.Types                         (status200)
import           Blaze.ByteString.Builder                   (copyByteString)
import           Control.Monad                                                  hiding (when)
import qualified Control.Monad                  as M        (when)
import           Database.HDBC.Sqlite3                      (connectSqlite3)
import qualified Database.HDBC                  as HDBC     (disconnect)
import           Data.FileEmbed                             (embedStringFile)
import           Data.String
import           System.Environment                         (getExecutablePath)
import           System.FilePath                            (dropFileName, (</>))
import qualified Graphics.UI.Threepenny         as UI
import           Graphics.UI.Threepenny.Core                                    hiding (get)
import qualified Graphics.UI.Threepenny.Core    as TP        (get)
import           Control.Concurrent                          (forkIO)
import           Data.Tuple
import           Data.Tuple.Extra                            ((***))
import           Data.Either

import           Type
import           GoogleTranslate
import           Lexin
import           Session
import           Db
import           Menu
 
databaseName =  "tiril.db" 

schema :: IsString a => a
schema = $(embedStringFile "./app/schema.sql")

serverPort = 3000
uiPort = 3001

main = do
    exeDir <- dropFileName <$> getExecutablePath
    let dbPath = exeDir </> databaseName
    putStrLn $ "DB path " ++ dbPath
    startOk <- dbExists dbPath
    startOk <- if not startOk then do
                    res <- dbCreate dbPath schema
                    case res of 
                        Left v -> do 
                            putStrLn . show $ v
                            return False
                        _ -> dbExists dbPath
               else return True
    if startOk then do
        putStrLn $ "AppUI at http://localhost:" ++ show uiPort
        let static = exeDir </> "static"
        putStrLn $ "Static dir " ++ show static
        let config = defaultConfig
                    { jsPort   = Just uiPort
                    , jsStatic = Just static
--                    , jsLog = const (return ()) 
                    }
        void $ forkIO $ startGUI config $ uiSetup
        
        putStrLn $ "HTTP-server at http://localhost:" ++ show serverPort
        run serverPort httpServer
    else return ()
 
httpServer :: Network.Wai.Request -> (Network.Wai.Response -> IO ResponseReceived) -> IO ResponseReceived
httpServer req respond = join $ respond <$>
    case pathInfo req of
        ("goo":[x]) -> do
            tx <- googleTranslateWithT . T.fromStrict $ x
            return $ either index index tx
        ("lex":[x]) -> do
            tx <- lexinTranslate . T.fromStrict $ x
            -- TODO: Currently you take merely 2 sections. Think of a proper extension to give out all available data
            return . index . take 2 $ tx
        ("add":[x]) -> do
            conn <- connectSqlite3 databaseName
            r <- addWord conn . T.fromStrict $ x
            HDBC.disconnect conn
            return $ either (index . show) (index . const "done") r
        _ -> return . index $ "Unknown command"
    where index :: Show a => a -> Network.Wai.Response
          index x = 
            responseBuilder status200 [ ("Content-Type", "text/html; charset=UTF-8") ] $ 
            mconcat $ map copyByteString [ "<p>", BU.fromString . US.ushow $ x, "</p>" ]

            
            
--bootstrapMenu :: UI Element
bootstrapMenu sessionHandler = do
    a <- UI.a    
    on UI.click a sessionHandler
    mkElement "nav" #. "navbar sticky-top navbar-expand-md navbar-light bg-light"
                    #+ [ UI.a #. "navbar-brand"
                              # set UI.href "#"
                              # set text "Tiril"
                       , UI.button #. "navbar-toggler"
                                   # set UI.type_ "button"
                                   # set (attr "data-toggle") "collapse"
                                   # set (attr "data-target") "#navbarSupportedContent"
                                   # set (attr "aria-controls") "navbarSupportedContent"
                                   # set (attr "aria-expanded") "false"
                                   # set (attr "aria-label") "Toggle navigation"
                                   #+ [UI.span #. "navbar-toggler-icon"]
                       , UI.div #. "collapse navbar-collapse"
                                # set UI.id_ "navbarSupportedContent"
                                #+ [ UI.ul #. "navbar-nav mr-auto"
                                           #+ [ UI.li #. "nav-item active"    -- Single link
                                                      #+ [ UI.a #. "nav-link"  
                                                                # set UI.href "#"
                                                                # set text "Some link"
                                                         ]
                                              , UI.li #. "nav-item dropdown"  -- Dropdown menu
                                                      #+ [ UI.a #. "nav-link dropdown-toggle"
                                                                # set UI.href "#"
                                                                # set UI.id_ "navbarDropdown"
                                                                # set (attr "role") "button"
                                                                # set (attr "data-toggle") "dropdown"
                                                                # set (attr "aria-haspopup") "true"
                                                                # set (attr "aria-expanded") "false"
                                                                # set text "Some dropdown"
                                                         , UI.div #. "dropdown-menu"
                                                                  # set (attr "aria-labelledby") "navbarDropdown"
                                                                  #+ [ element a #. "dropdown-item"
                                                                                 # set UI.href "#"
                                                                                 # set text "Session"
                                                                     , UI.a #. "dropdown-item"
                                                                            # set UI.href "#"
                                                                            # set text "Some another action"
                                                                     , UI.div #. "dropdown-divider"
                                                                     , UI.a #. "dropdown-item"
                                                                            # set UI.href "#"
                                                                            # set text "Some once another action"
                                                                     ]
                                                         ]
                                              , UI.li #. "nav-item"  -- Disabled
                                                      #+ [ UI.a #. "nav-link disabled"
                                                                # set UI.href "#"
                                                                # set text "Disabled" 
                                                         ]
                                              ]
                                   , UI.form #. "form-inline my-2 my-lg-0"
                                             #+ [ UI.input #. "form-control mr-sm-2"
                                                           # set UI.type_ "search"
                                                           # set (attr "placeholder") "Search"
                                                           # set (attr "aria-label") "Search"
                                                , UI.button #. "btn btn-outline-success my-2 my-sm-0"
                                                            # set UI.type_ "submit"
                                                            # set text "Search"
                                                ]
                                   ]
                       ]

            
uiSetup :: Window -> UI ()
uiSetup win = do
    return win # set UI.title "Tiril"
    -- CSS: Bootstrap + Dragula + own
    void $ getHead win #+ [ UI.meta # set UI.name "viewport"
                                    # set UI.content "width=device-width, initial-scale=1, shrink-to-fit=no"
                          , UI.link # set UI.rel "stylesheet"
                                    # set UI.href "/static/css/bootstrap.min.css" ]
    UI.addStyleSheet win "dragula.min.css"
    UI.addStyleSheet win "app.css"

    -- Adding menu
    mainMenu <- bootstrapMenu viewSession
{--    mainMenu <- runMenu $ do 
        createTopBarMenu "Tiril"
        menu "Review"
        subMenu "Session" (viewSession)
        menu "Export"
        subMenu "Anki" (empty "Anki!? Not yet, but it's coming ...")
        subMenu "Memrise"  (empty "Memrise!? Not yet, but it's coming also ...") --}
                
    void $ getBody win #+ [element mainMenu, createMainWindow]
                                        
    -- JS: Bootstrap (+ Popper) + Dragula + own
    void $ getBody win #+ [ mkElement "script" # set UI.src "/static/js/vendor/popper.min.js"
                          , mkElement "script" # set UI.src "/static/js/vendor/bootstrap.min.js"
                          , mkElement "script" # set UI.src "/static/js/vendor/dragula.min.js"
                          , mkElement "script" # set UI.src "/static/js/app.js" ]
    where createMainWindow :: UI Element
          createMainWindow = UI.div #. "tiril-main-window"

          getWindows :: String -> UI [Element]
          getWindows className = flip getElementsByClassName className =<< askWindow

          deleteWindows :: String -> UI ()
          deleteWindows className =
            void . sequence =<< map delete <$> getWindows className
          
          -- The window must exist already
          getMainWindow :: UI Element
          getMainWindow = head <$> getWindows "tiril-main-window"
          
          clearMainWindow = getMainWindow # set children []
            
          empty msg = const $ do
            clearMainWindow
            getMainWindow #+ [ UI.div # set text msg ]
            
          viewSession = const $ do
            sessionWords <- liftIO $ do
                conn <- connectSqlite3 databaseName
                res <- (either (flip (:) [] . T.pack . show) id) <$> getWords conn
                HDBC.disconnect conn
                return res
            clearMainWindow
            void $ getMainWindow #+ [ UI.div #. "tiril-word-list dragula-container" 
                                             #+ (makeCard . T.unpack <$> sessionWords) ]

          jsToggleCard :: Element -> JSFunction Int
          jsToggleCard = ffi "uiToggleCard(%1)"
          
          makeCard :: String -> UI Element
          makeCard word = do
                card <- UI.div #. "tiril-word"
                               #+ [ UI.img #. "draggable" 
                                           # set UI.src "static/ico/ic_reorder_black_24px.svg" --"static/ico/drag.svg"
                                  , UI.a # set text word ]
                on UI.click card $ const $ do
                    deleteWindows "tiril-right-pane"
                    isSelectedNow <- callFunction $ jsToggleCard card
                    if isSelectedNow /= 0 then do
                        tr <- liftIO $ do
                            (lex :: [[LexinWord]]) <- lexinTranslate . T.pack $ word
                            gooRes <- googleTranslateWithT . T.pack $ word
                            -- TODO: Think of better ways to output the error
                            M.when (isLeft gooRes) $ do
                                -- We are not afraid of matching Left only,
                                -- since we had the guard "when" before coming here
                                let (Left x) = gooRes
                                putStrLn . T.unpack $ x
                            let (goo :: [Tir]) = either (const []) id gooRes
                            return (goo, lex)
                        
                        let (ts::[UI Element]) = uncurry (:) . (createWordPack *** ((<$>) createWordPack)) $ tr
                        void $ getMainWindow #+ [UI.div #. "tiril-right-pane" 
                                                        #+ ts]
                    else return ()
                return card
           
          createWordPack :: Translator a => [a] -> UI Element
          createWordPack ws = UI.div #. "tiril-detail-container"
                                     #+ (:) (mkElement "header") (createWord <$> ws)
          createWord :: Translator a => a -> UI Element
          createWord w = UI.div #. "tiril-translation-word draggable" 
                                #+ [ UI.img # set UI.src "static/ico/drag.svg"
                                   , UI.div #+ [ UI.span # set text (target w) 
                                               , UI.sub # set text (verb w ++ " | " ++ lang w ++ " | " ++ iam w) ] ]