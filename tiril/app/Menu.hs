{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}
module Menu 
(createTopBarMenu, createStickyMenu, menu, subMenu, runMenu)
where

import qualified Graphics.UI.Threepenny         as UI
import           Graphics.UI.Threepenny.Core                                    hiding (get)
import           Control.Monad.State

type Setter = ([UI Element] -> UI Element)
type MenuState = ( Setter, [ (Setter, [UI Element]) ] )

newtype TopBar a = TBar (State MenuState a) 
                        deriving (Functor, Applicative, Monad)
                   
newtype TopBarSticky a = TBarSticky (State MenuState a) 
                        deriving (Functor, Applicative, Monad)

-- Typical usage: runMenu $ createTopBarMenu "Product" 
--                                   >> menu "Menu1" >> subMenu "Submenu1" handler1 >> subMenu "Submenu2" handler2 
--                                   >> menu "Menu2" >> menu "Menu3"  ...

class (Monad m) => TirilMenu m where
    top :: String -> m (UI Element)
    menu :: String -> m (UI Element)
    subMenu :: String -> (() -> UI void) -> m (UI Element)
    runMenu :: m (UI Element) -> UI Element

-- TODO: Check the monad with >>= as well
instance TirilMenu (TopBar) where
    top name = TBar $ do
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
    menu name = TBar $ createDefaultTopBarMenu "menu vertical" name
    subMenu name handler = TBar $ createDefaultTopBarSubMenu "menu vertical" name handler
    runMenu (TBar m) = evalState m ((\x->undefined), []) -- initial state is never used

instance TirilMenu (TopBarSticky) where
    top name = TBarSticky $ do
        put (head name, [])
        return $ head name []
        where 
            head name items = 
                UI.div  # set (attr "data-sticky-container") ""
                        #+ [UI.div # set (attr "data-sticky") "" 
                                   # set (attr "data-options") "marginTop:0;"
                                   #+ [ UI.div #. "title-bar" 
                                               # set (attr "data-responsive-toggle") "example-menu"
                                               # set (attr "data-hide-for") "medium"
                                               #+ [UI.button #. "menu-icon"
                                                             # set (attr "type") "button"
                                                             # set (attr "data-toggle") "example-menu"
                                                  ,UI.div #. "title-bar-title" 
                                                          # set text "Menu" ]
                                      , UI.div #. "top-bar"
                                               # set (attr "id") "example-menu"
                                               #+ [UI.ul #. "vertical medium-horizontal dropdown menu" 
                                                         # set (attr "data-responsive-menu") "accordion medium-dropdown"
                                                         #+ ([UI.li #. "menu-text" 
                                                                    # set text name ] ++ items) ] ]]

    menu name = TBarSticky $ createDefaultTopBarMenu "menu vertical nested" name
    subMenu name handler = TBarSticky $ createDefaultTopBarSubMenu "menu vertical nested" name handler
    runMenu (TBarSticky m) = evalState m ((\x->undefined), []) -- initial state is never used

{--- Constructors ---}
createTopBarMenu :: String -> TopBar (UI Element)
createTopBarMenu = top 

createStickyMenu :: String -> TopBarSticky (UI Element)
createStickyMenu = top 
    
{--- Utility functions  ---}
utilLiElem className name items = UI.li #+ [UI.a # set (attr "href") ("#") # set text name, UI.ul #. className #+ items]

utilApplyFunction :: [(Setter, [UI Element])] -> [UI Element]
utilApplyFunction [] = []
utilApplyFunction ((fn, ls):xs) = (fn ls):utilApplyFunction xs
    
createDefaultTopBarMenu :: String -> String -> State MenuState (UI Element)
createDefaultTopBarMenu className name = do
    (head, ls) <- get
    let new = ls ++ [(utilLiElem className name, [])]
    put (head, new)
    return . head $ (utilApplyFunction ls) ++ [(utilLiElem className name [])]
        
createDefaultTopBarSubMenu :: String -> String -> (() -> UI void) -> State MenuState (UI Element)
createDefaultTopBarSubMenu className name handler = do
    (head_, ls) <- get
    let (prev_, (lst_fn, lst_ls)) = case null ls of
            True -> ([], (utilLiElem className "Forgot something?", []))
            _ -> (init ls, last ls)
    let new = lst_ls ++ [elem name handler]
    put (head_, prev_ ++ [(lst_fn, new)])
    (head2, ls2) <- get
    return . head2 $ utilApplyFunction ls2
    where 
        elem txt handler = do
            a <- UI.a # set (attr "href") ("#") # set text txt
            on UI.click a handler
            UI.li #+ [element a]
        