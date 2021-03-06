{-# LANGUAGE FlexibleContexts, PackageImports, OverloadedStrings #-}
module Main where

import Clckwrks             (ClckwrksConfig(..), ClckState, plugins)
import Clckwrks.Authenticate.Plugin (authenticatePlugin)
import Clckwrks.GetOpts     (parseArgs, clckwrksOpts)
import Clckwrks.Server      (simpleClckwrks)
import Clckwrks.Plugin      (clckPlugin)
import Clckwrks.Page.Plugin (pagePlugin)
import Clckwrks.Redirect.Plugin (redirectPlugin)
import Data.Text            (Text)
import Web.Plugins.Core     (Rewrite(..), initPlugin, setRewriteFn, setTheme)
import System.Environment   (getArgs)
-- we use 'PackageImports' because the 'Theme' module is supplied by multiple packages
import "clckwrks-theme-bootstrap" Theme (theme)
-- import Theme (theme)

------------------------------------------------------------------------------
-- ClckwrksConfig
------------------------------------------------------------------------------

-- | default configuration. Most of these options can be overridden on
-- the command-line accept for 'clckInitHook'.
clckwrksConfig :: ClckwrksConfig
clckwrksConfig = ClckwrksConfig
    { clckHostname        = "localhost"  -- hostname of the server
    , clckHidePort        = False        -- should the port number be used in generated URLs
    , clckPort            = 8000         -- port to listen on
    , clckTLS             = Nothing      -- disable TLS by default
    , clckJQueryPath      = "/usr/share/javascript/jquery"  -- directory containing 'jquery.js'
    , clckJQueryUIPath    = ""           -- directory containing 'jquery.js'
    , clckJSTreePath      = "/usr/share/clckwrks/jstree"  -- directory containing 'jquery.jstree.js'
    , clckJSON2Path       = "/usr/share/clckwrks/json2"   -- directory containing 'json2.js'
    , clckTopDir          = Nothing      -- directory to store database, uploads, and other files
    , clckEnableAnalytics = False        -- enable Google Analytics
    , clckInitHook        = initHook     -- init hook that loads theme and plugins
    }

------------------------------------------------------------------------------
-- initHook
------------------------------------------------------------------------------

-- | This 'initHook' is used as the 'clckInitHook' field in
-- 'ClckwrksConfig'.
--
-- It will be called automatically by 'simpleClckwrks'. This hook
-- provides an opportunity to explicitly load a theme and some
-- plugins.
--
-- Note that the we generally always init 'clckPlugin' here.
initHook :: Text           -- ^ baseURI, e.g. http://example.org
         -> ClckState      -- ^ current 'ClckState'
         -> ClckwrksConfig -- ^ the 'ClckwrksConfig'
         -> IO (ClckState, ClckwrksConfig)
initHook baseURI clckState cc =
    do let p = plugins clckState
       initPlugin p "" clckPlugin
       initPlugin p "" redirectPlugin
       setRewriteFn p (Just $ (pure rewriteFilter, pure unRewriteFilter))
       initPlugin p "http://localhost:8000" authenticatePlugin
       initPlugin p "" pagePlugin
       setTheme p (Just theme)
       return (clckState, cc)
         where
           rewriteFilter :: [Text] -> [(Text, Maybe Text)] -> Maybe (Rewrite, [Text], [(Text, Maybe Text)])
           rewriteFilter ["login"] params = Just (Rewrite, ["authenticate","login"], params)
           rewriteFilter ["admin"] params = Just (Rewrite, ["clck","admin", "console"], params)
           rewriteFilter ["settings"] params = Just(Redirect Nothing, ["clck","admin","edit-settings"], params)
           rewriteFilter ["clckwrks"] params = Just( Redirect (Just "http://www.clckwrks.com"), [], params)
           rewriteFilter paths     params = Nothing

           unRewriteFilter :: [Text] -> [(Text, Maybe Text)] -> Maybe ([Text], [(Text, Maybe Text)])
           unRewriteFilter ["authenticate","login"] params = Just (["login"], params)
           unRewriteFilter ["clck","admin", "console"] params = Just (["admin"], params)
           unRewriteFilter paths     params = Just (paths, params)

------------------------------------------------------------------------------
-- main
------------------------------------------------------------------------------

main :: IO ()
main =
    do args <- getArgs
       f    <- parseArgs (clckwrksOpts clckwrksConfig) args
       putStrLn $  "listening on " ++ show (clckPort clckwrksConfig)
       simpleClckwrks =<< f clckwrksConfig
