module BootstrapMenu 
(evalMenu, navbar, search, link, newDropdown, dropdownDivider, dropdownItem, dropdownNamedDivider)
where

import           Control.Monad                              (void)
import           Control.Monad.State        
import qualified Graphics.UI.Threepenny         as UI       
import           Graphics.UI.Threepenny.Core                hiding (get)
            
data Navbar         = Bar [Menu]
                    | Search
data Menu           = Link String String
                    | Dropdown String [DropdownItem] 
data DropdownItem   = Divider
                    | NamedDivider String
                    | Item String (() -> UI ())
data Container      = Container String [Navbar]

navbar :: String -> State Container Container
navbar name = do
    put $ Container name []
    get

search :: State Container Container
search = do
    (Container name ls) <- get
    put . Container name $ Search:ls
    get

link :: String -> String -> State Container Container
link name ref = do
    (Container n ls) <- get
    let new = case ls of
            ([])            -> [Bar [empty]]
            a@(Search:ls)   -> (Bar [empty]) : a
            ((Bar bs):ls)   -> (Bar $ empty : bs) : ls
    put . Container n $ new
    get
    where empty = Link name ref
    
newDropdown :: String -> State Container Container
-- navbar .. >>= newDropdown ..
newDropdown name = do
    (Container n ls) <- get
    let new = case ls of
            ([])            -> [Bar [empty]]
            a@(Search:ls)   -> (Bar [empty]) : a
            ((Bar bs):ls)   -> (Bar $ empty : bs) : ls
    put . Container n $ new
    get
    where empty = Dropdown name []

dropdownDivider :: State Container Container
-- navbar .. >>= newDropdown >>= dropdownDivider >>= newDropdown ..
dropdownDivider = do
    (Container n ls) <- get
    let new = case ls of
            ((Bar ((Dropdown dname ds):bs)):ls) -> (Bar $ (Dropdown dname $ Divider:ds):bs):ls
    put . Container n $ new
    get

dropdownNamedDivider :: String -> State Container Container
-- navbar .. >>= newDropdown >>= dropdownNamedDivider >>= newDropdown ..
dropdownNamedDivider cap = do
    (Container n ls) <- get
    let new = case ls of
            ((Bar ((Dropdown dname ds):bs)):ls) -> (Bar $ (Dropdown dname $ NamedDivider cap:ds):bs):ls
    put . Container n $ new
    get
    
-- In spite that elsewhere "UI void" normally would mean that a function never returns (or just throws),
-- in the case of Threepenny the function still must return.
-- It seems that the author uses this kind of syntax to proclaim that he just doesn't care of the function's result.
-- We'll take advantage of it and just ignore the result as well, 
-- otherwise we would have to introduce an unused type variable throughout all the ADTs above, which is excessive
dropdownItem :: String -> (() -> UI any) -> State Container Container
dropdownItem name handler = do
    (Container n ls) <- get
    let new = case ls of
            ((Bar ((Dropdown dname ds):bs)):ls) -> (Bar $ (Dropdown dname $ item:ds):bs):ls
    put . Container n $ new
    get
    where item = Item name (void . handler)

evalMenu :: State Container Container -> UI Element
evalMenu a = runContainer $ evalState a (Container "" [])
    
runContainer (Container name navs) = do 
    mkElement "nav" #. "navbar sticky-top navbar-expand-md navbar-light bg-light"
                    #+ [ UI.a #. "navbar-brand"
                              # set UI.href "#"
                              # set text name
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
                                -- menu is stored as if in a stack, meaning in a backward order
                                #+ (runNavbar <$> reverse navs) ]
                                
runNavbar Search = 
    UI.form #. "form-inline my-2 my-lg-0" -- search
        #+ [ UI.input #. "form-control mr-sm-2"
            # set UI.type_ "search" 
            # set (attr "placeholder") "Search"
            # set (attr "aria-label") "Search"
           , UI.button #. "btn btn-outline-success my-2 my-sm-0"
            # set UI.type_ "submit"
            # set text "Search" ]
            
runNavbar (Bar mns) = 
    UI.ul #. "navbar-nav mr-auto"
        #+ (runMenu <$> reverse mns)
        
runMenu (Link name ref) = 
    UI.li #. "nav-item active"    -- Single link
        #+ [ UI.a #. "nav-link"  
            # set UI.href ref
            # set text name ] 
                                
runMenu (Dropdown name ls) = 
    UI.li 
        #. "nav-item dropdown"  -- Dropdown menu
        #+ [ UI.a 
            #. "nav-link dropdown-toggle"
            # set UI.href "#"
            # set UI.id_ "navbarDropdown"
            # set (attr "role") "button"
            # set (attr "data-toggle") "dropdown"
            # set (attr "aria-haspopup") "true"
            # set (attr "aria-expanded") "false"
            # set text name
           , UI.div #. "dropdown-menu"
            # set (attr "aria-labelledby") "navbarDropdown"
            #+ (runDropdown <$> reverse ls) ]

runDropdown Divider = 
    UI.div #. "dropdown-divider"

runDropdown (NamedDivider s) = 
    UI.h6 #. "dropdown-header" # set text s
    
runDropdown (Item name handler) = do
    a <- UI.a    
    on UI.click a handler    
    element a #. "dropdown-item"
        # set UI.href "#"
        # set text name