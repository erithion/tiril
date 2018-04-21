{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}
module Menu 
(runTopBarMenu, topbar, tmenu, tsubmenu)
where

import qualified Graphics.UI.Threepenny         as UI
import           Graphics.UI.Threepenny.Core                                    hiding (get)
import           Control.Monad.State

type TopBarFn = ([UI Element] -> UI Element)
type TopBarState = ( TopBarFn, [ (TopBarFn, [UI Element]) ] )
newtype TopBar a = TBar (State TopBarState a) 
                   deriving (Functor, Applicative, Monad)

-- Typical usage: runTopBarMenu $ topbar "Product" 
--                                   >> tmenu "Menu1" >> tsubmenu "Submenu1" handler1 >> tsubmenu "Submenu2" handler2 
--                                   >> tmenu "Menu2" >> tmenu "Menu3"  ...

-- TODO: Make menu polymorphic - declare a class Menu and move out topbar, tmenu etc.
-- Align TopBar monad with >>= as well
topbar :: String -> TopBar (UI Element)
topbar name = TBar $ do
    put (head name, [])
    return $ head name []
    where 
        head name items = 
            UI.div #. "top-bar" 
                   #+ [UI.div #. "top-bar-left" 
                              #+ [UI.ul #. "dropdown menu" 
                                        # set (attr "data-dropdown-menu") ""
                                        #+ ([UI.li #. "menu-text" 
                                                   # set text name ] ++ items) ]]
                                                   
tmenu :: String -> TopBar (UI Element)
tmenu name = TBar $ do
    (head, ls) <- get
    let new = ls ++ [(li name, [])]
    put (head, new)
    return . head $ (apply ls) ++ [(li name [])]
    where
        li name items = UI.li #+ [UI.a # set (attr "href") ("#") # set text name, UI.ul #. "menu vertical" #+ items]
        apply :: [(TopBarFn, [UI Element])] -> [UI Element]
        apply [] = []
        apply ((fn, ls):xs) = (fn ls):apply xs

-- TODO: Restrict types        
--tsubmenu :: String -> (e -> UI void) -> TopBar (UI Element)
tsubmenu name handler = TBar $ do
    (head_, ls) <- get
    let (prev_, (lst_fn, lst_ls)) = case null ls of
                True -> ([], (li_to_delete "Forgot something?", []))
                _ -> (init ls, last ls)
    let new = lst_ls ++ [elem name handler]
    put (head_, prev_ ++ [(lst_fn, new)])
    (head2, ls2) <- get
    return . head2 $ apply ls2
    where 
            -- TODO: it's a duplicate. move out
            li_to_delete name items = UI.li #+ [UI.a # set (attr "href") ("#") # set text name, UI.ul #. "menu vertical" #+ items]
--            elem :: forall e void. String -> (e -> UI void) -> UI Element
            elem txt handler = do
                    a <- UI.a # set (attr "href") ("#") # set text txt
                    on UI.click a handler
                    UI.li #+ [element a]
            -- TODO: it's a duplicate. move out
            apply :: [(TopBarFn, [UI Element])] -> [UI Element]
            apply [] = []
            apply ((fn, ls):xs) = (fn ls):apply xs

runTopBarMenu :: TopBar (UI Element) -> UI Element
runTopBarMenu (TBar m) = evalState m ((\x->undefined), []) -- initial state is never used
            