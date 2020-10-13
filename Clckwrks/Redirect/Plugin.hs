{-# LANGUAGE RecordWildCards, FlexibleContexts, FlexibleInstances, OverloadedStrings #-}
module Clckwrks.Redirect.Plugin where

import Clckwrks                     ( ClckwrksConfig(clckTopDir), ClckState(plugins), ClckT(..), ClckURL, ClckPlugins, Theme
                                    , Role(..), ClckPluginsSt, addAdminMenu, addNavBarCallback, addPreProc, query, update
                                    )
import Clckwrks.Acid                (GetUACCT(..), SetUACCT(..))
import Clckwrks.Plugin              (clckPlugin)
import Clckwrks.Redirect.Acid       (RedirectState, initialRedirectState)
import Clckwrks.Redirect.Monad      (RedirectConfig(..), runRedirectT)
import Clckwrks.Redirect.Route      (routeRedirect)
import Clckwrks.Redirect.URL        (RedirectURL(..), RedirectAdminURL(..))
import Clckwrks.Redirect.Types      ()
import Control.Applicative          ((<$>))
import Control.Monad.State          (get)
import Data.Acid                    (AcidState)
import Data.Acid.Advanced           (update', query')
import Data.Acid.Local              (createCheckpointAndClose, openLocalStateFrom,)
import Data.Char              (ord)
import Data.List              (intersperse)
import Data.String            (fromString)
import qualified Data.Text    as Text
import Data.Text                    (Text)
import Data.Text.Lazy         (toStrict)
import qualified Data.Text.Lazy     as TL
import Data.Text.Lazy.Builder (Builder, fromText, singleton, toLazyText)
import Data.Typeable                (Typeable)
import Data.Maybe                   (fromMaybe)
import Data.Set                     (Set)
import qualified Data.Set           as Set
import Numeric                (showIntAtBase)
import Happstack.Server             (ServerPartT, Response, seeOther, notFound, toResponse)
-- import System.Directory             (createDirectoryIfMissing)
import System.FilePath              ((</>))
import Web.Routes                   (toPathSegments, parseSegments, withRouteT, fromPathSegments)
import Web.Plugins.Core             (Plugin(..), Plugins(..), When(..), addCleanup, addHandler, addPostHook, addPluginRouteFn, initPlugin, getConfig, getPluginRouteFn)

redirectHandler :: (RedirectURL -> [(Text, Maybe Text)] -> Text)
                -> RedirectConfig
                -> ClckPlugins
                -> [Text]
                -> ClckT ClckURL (ServerPartT IO) Response
redirectHandler showRedirectURL redirectConfig plugins paths =
    case parseSegments fromPathSegments paths of
      (Left e)  -> notFound $ toResponse (show e)
      (Right u) ->
          ClckT $ withRouteT flattenURL $ unClckT $ runRedirectT redirectConfig $ routeRedirect u
    where
      flattenURL ::   ((url' -> [(Text, Maybe Text)] -> Text) -> (RedirectURL -> [(Text, Maybe Text)] -> Text))
      flattenURL _ u p = showRedirectURL u p

redirectInit :: ClckPlugins
         -> IO (Maybe Text)
redirectInit plugins =
    do (Just redirectShowFn) <- getPluginRouteFn plugins (pluginName redirectPlugin)
       (Just clckShowFn)     <- getPluginRouteFn plugins (pluginName clckPlugin)
       mTopDir <- clckTopDir <$> getConfig plugins
       let basePath = maybe "_state" (\td -> td </> "_state") mTopDir -- FIXME
           redirectDir  = maybe "_redirect" (\td -> td </> "_redirect") mTopDir

       irs  <- initialRedirectState
       acid <- openLocalStateFrom (basePath </> "redirect") irs
       addCleanup plugins Always (createCheckpointAndClose acid)

       let redirectConfig = RedirectConfig { redirectState     = acid
                                           , redirectClckURL   = clckShowFn
                                           }

--       addPreProc plugins (redirectCmd acid redirectShowFn)
--       addNavBarCallback plugins (navBarCallback acid redirectShowFn)
       addHandler plugins (pluginName redirectPlugin) (redirectHandler redirectShowFn redirectConfig)
--       addPostHook plugins (migrateUACCT acid)

       return Nothing

addRedirectAdminMenu :: ClckT url IO ()
addRedirectAdminMenu =
    do p <- plugins <$> get
       -- (Just redirectShowURL) <- getPluginRouteFn p (pluginName redirectPlugin)
       addAdminMenu ( "Redirect/Rewrites"
                    , [ (Set.fromList [Administrator, Editor], "Redirects", "")
                      ]
                    )
       pure ()
{-
       let newRedirectURL    = pageShowURL (PageAdmin NewPage) []
           pagesURL      = pageShowURL (PageAdmin Pages) []
           feedConfigURL = pageShowURL (PageAdmin EditFeedConfig) []
       addAdminMenu ("Pages/Posts"
                    , [ (Set.fromList [Administrator, Editor], "New Page/Post"   , newRedirectURL)
                      , (Set.fromList [Administrator, Editor], "Edit Page/Post"  , pagesURL)
                      , (Set.fromList [Administrator, Editor], "Edit Feed Config", feedConfigURL)
                      ]
                    )
-}
redirectPlugin :: Plugin RedirectURL Theme (ClckT ClckURL (ServerPartT IO) Response) (ClckT ClckURL IO ()) ClckwrksConfig ClckPluginsSt
redirectPlugin = Plugin
    { pluginName           = "redirect"
    , pluginInit           = redirectInit
    , pluginDepends        = ["clck"]
    , pluginToPathSegments = toPathSegments
    , pluginPostHook       = addRedirectAdminMenu
    }

plugin :: ClckPlugins -- ^ plugins
       -> Text        -- ^ baseURI
       -> IO (Maybe Text)
plugin plugins baseURI =
    initPlugin plugins baseURI redirectPlugin

-- | initialize the redirect plugin
--
-- we can not use the standard `initPlugin` function for the redirect plugin because we want to intercept URLs higher up the chain.
initRedirectPlugin ::
              Plugins Theme (ClckT ClckURL (ServerPartT IO) Response) (ClckT ClckURL IO ()) ClckwrksConfig ClckPluginsSt    -- ^ 'Plugins' handle
           -> Text                              -- ^ base URI to prepend to generated URLs
           -> IO (Maybe Text)                   -- ^ possible error message
initRedirectPlugin plugins baseURI =
    do -- putStrLn $ "initializing " ++ (Text.unpack pluginName)
       let (Plugin{..}) = redirectPlugin
       addPluginRouteFn plugins pluginName baseURI pluginToPathSegments -- (\u p ->  {- <> "/" <> {- pluginToPathInfo u <> -} paramsToQueryString (map (\(k, v) -> (k, fromMaybe mempty v)) p)-})
       addPostHook plugins pluginPostHook
       pluginInit plugins

paramsToQueryString :: [(Text, Text)] -> Text
paramsToQueryString [] = mempty
paramsToQueryString ps = toStrictText $ "?" <> mconcat (intersperse "&" (map paramToQueryString ps) )
    where
      toStrictText = toStrict . toLazyText

      isAlphaChar :: Char -> Bool
      isAlphaChar c    = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

      isDigitChar :: Char -> Bool
      isDigitChar c    = (c >= '0' && c <= '9')

      isOk :: Char -> Bool
      isOk c = isAlphaChar c || isDigitChar c || elem c (":@$-_.~" :: String)

      escapeChar c
          | c == ' '  = singleton '+'
          | isOk c    = singleton c
          | otherwise = "%" <>
                        let hexDigit n
                                | n <= 9 = head (show n)
                                | n == 10 = 'A'
                                | n == 11 = 'B'
                                | n == 12 = 'C'
                                | n == 13 = 'D'
                                | n == 14 = 'E'
                                | n == 15 = 'F'
                        in case showIntAtBase 16 hexDigit (ord c) "" of
                             []  -> "00"
                             [x] -> fromString ['0',x]
                             cs  -> fromString cs

      escapeParam :: Text -> Builder
      escapeParam p = Text.foldr (\c cs -> escapeChar c <> cs) mempty p

      paramToQueryString :: (Text, Text) -> Builder
      paramToQueryString (k,v) = (escapeParam k) <> "=" <> (escapeParam v)
