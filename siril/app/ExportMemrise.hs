{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExportMemrise 
(memriseExportWindow)
where

import           Data.Aeson                     as JS
import           GHC.Generics
import qualified Graphics.UI.TinyFileDialogs    as Dlg
import           Data.Maybe
import qualified Graphics.UI.Threepenny         as UI
import           Graphics.UI.Threepenny.Core                                    hiding (get)
import qualified Graphics.UI.Threepenny.Core    as TP        (get)
import qualified Data.Text.Lazy                 as T
import qualified Data.Text                      as TS
import qualified Data.Text.IO                   as TS
import qualified Control.Monad                  as M         (when)
import qualified Data.Map.Strict                as Map
import           Data.Tuple.Extra                            ((***), (&&&))
import           Control.Monad                               (void)
import           GHC.IO.Encoding                             (utf8, setLocaleEncoding)
import           Data.List                                   (transpose, intercalate)

import           Type
import           Windows
import           Session

data Dataset = Dataset
    { exportSource :: T.Text
    , exportTargets :: [[Data]] 
    } deriving (Generic, Show)
    
data Data = Data
    { exportTarget :: T.Text
    , exportLang :: T.Text
    , exportDict :: T.Text
    , exportLem :: T.Text 
    } deriving (Generic, Show)
    
instance FromJSON Data
instance FromJSON Dataset

instance Export Dataset where
    export delimiter dat = 
        let (columns :: [[Data]]) = {- transpose . -} exportTargets $ dat
            (ret :: [T.Text]) = map (T.intercalate ";" . map extract) $ columns
        in T.intercalate delimiter $ ret
        where extract :: Data -> T.Text
              extract d = exportTarget d
            
jsInitExport :: String -> UI ()
jsInitExport = runFunction . ffi "initExport(%1)"
jsInitScrollbar :: String -> UI ()
jsInitScrollbar = runFunction . ffi "var ps = new PerfectScrollbar(%1)"

jsGetDataFromTable :: String -> UI JS.Value
jsGetDataFromTable = callFunction . ffi "finalCollect(%1)"

memriseExportWindow = do
    clearMainWindow
    exports <- liftIO getExports
    -- Transforming to the Map for the sake of merging the same source words into single groups; and back to the List
    let groups = Map.assocs . Map.fromListWith (++) $ map ( source &&& (:[]) ) exports

    tagsRow <- 
        UI.tr #+ [ UI.td #+ [ 
            mkElement "selectize"
                # set UI.type_ "text" 
                # set (attr "placeholder") "Enter filtering tag ..."
            ] ]
    table <- UI.table 
        # set UI.id_ "export"
        #+ (:) (element tagsRow) (concat . map createHtmlTableRow $ groups)
    exportButton <- UI.button # set UI.type_ "button" #. "btn btn-light btn-lg ml-3 mt-3" # set text "Export"
            
    on UI.click exportButton $ const $ do
        (dataset :: Result [Dataset]) <- fromJSON <$> jsGetDataFromTable "export"
        case dataset of 
            Error   err -> showModal "Memrise" ("JSON error: " ++ err)
            Success dat -> do
                res <- liftIO $ do
                    filename <- Dlg.saveFileDialog "Save export file" "" ["*.htx"] "Tiril export"
                    flip (maybe $ return False) filename $ \name -> do
                        setLocaleEncoding utf8
                        TS.writeFile (TS.unpack name) (T.toStrict . T.unlines . map (export "\t") $ dat)
                        return True
                void . M.when res $ showModal "Memrise" "Exporting has been completed successfully!"
-- TODO: style and add this delimiter toggle
--    input <- UI.div #. "toggle toggle-modern ml-5"
    getMainWindow #+ 
        [ UI.link 
            # set UI.rel "stylesheet"
            # set UI.href "/static/selectize.default.css"
        , UI.link 
            # set UI.rel "stylesheet"
            # set UI.href "/static/toggles-full.css"
        , UI.link 
            # set UI.rel "stylesheet"
            # set UI.href "/static/export.css"
        , mkElement "script" # set UI.src "/static/selectize.min.js"
        , UI.div #. "flex-column" -- to display the button and the table in a column
            #+ [ UI.div #. "d-flex flex-row align-items-baseline w-50" 
                    #+ [ element exportButton
--                       , element input
                       ]
               , mkElement "divider"
               , element table ]
        , mkElement "script" # set UI.src "/static/toggles.min.js"
        , mkElement "script" # set UI.src "/static/export.js" 
        ]
    jsInitExport "export"
    jsInitScrollbar ".tiril-main-window"
 
createHtmlTableRow :: Translator a => (String, [a]) -> [UI Element]
createHtmlTableRow (cap, xs) = 
    let sourceRow = 
            [ UI.tr #+ [UI.td #+ [
                mkElement "translation-group-title" # set text cap
            ] ] ]
        translation x = mkElement "translation"
            #. "container row pt-1 pb-1"
            #+  [ mkElement "word" #. "col align-self-center" # set text (target x)
                , UI.div #. "col-auto row d-flex justify-content-end"
                    #+  [ mkElement "caps" #. "col d-flex flex-wrap justify-content-end align-content-center"
                            #+  [ mkElement "lem" #. "col-md-auto mr-1 mb-1 badge" # set text (verb x)
                                , mkElement "lang" #. "col-md-auto mr-1 mb-1 badge" # set text (lang x)
                                , UI.div #. "w-100"
                                , mkElement "dict" #. "col-md-auto mr-1 mb-1 badge" # set text (iam x)
                                ]
                        , mkElement "close" #. "col-auto m-0 p-0 align-self-center" # set text "✖"
                        ]
                ]
        targetRow = [ UI.tr #+ [ UI.td #+ (map translation xs) ] ]
    in sourceRow ++ targetRow