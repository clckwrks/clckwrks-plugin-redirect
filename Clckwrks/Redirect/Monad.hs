{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RecordWildCards, TypeFamilies, TypeSynonymInstances #-}
module Clckwrks.Redirect.Monad where

import Control.Applicative           ((<$>))
import Control.Monad                 (foldM)
import Control.Monad.Fail            (MonadFail(fail))
import Control.Monad.Reader          (MonadReader(ask,local), ReaderT(runReaderT))
import Control.Monad.State           (StateT, put, get, modify)
import Control.Monad.Trans           (MonadIO(liftIO))
import qualified Data.Text.Lazy      as LT
import Clckwrks.Acid                 (GetAcidState(..))
import Clckwrks.Monad                (Content(..), ClckT(..), ClckFormT, ClckState(..), ClckPluginsSt(..), mapClckT, runClckT, withRouteClckT, getPreProcessors)
import Clckwrks.URL                  (ClckURL)
import Clckwrks.Redirect.Acid        (RedirectState(..))
import Clckwrks.Redirect.Types       ()
import Clckwrks.Redirect.URL         (RedirectURL(..), RedirectAdminURL(..))
import Clckwrks.Redirect.Types       ()
import Clckwrks.Plugin               (clckPlugin)
import Control.Monad.Trans           (lift)
import Data.Acid                     (AcidState)
import Data.Data                     (Typeable)
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as TL
import Happstack.Server              (Happstack, Input, ServerPartT)
import HSP.XMLGenerator
import HSP.XML
import Text.Reform                   (CommonFormError, FormError(..))
import Web.Plugins.Core              (Plugin(..), getConfig, getPluginsSt, getPluginRouteFn)
import Web.Routes                    (RouteT(..), showURL, withRouteT)

data RedirectConfig = RedirectConfig
    { redirectState        :: AcidState RedirectState
    , redirectClckURL      :: ClckURL -> [(T.Text, Maybe T.Text)] -> T.Text
    }

type RedirectT m = ClckT RedirectURL (ReaderT RedirectConfig m)
type RedirectT' url m = ClckT url (ReaderT RedirectConfig m)
type RedirectM   = ClckT RedirectURL (ReaderT RedirectConfig (ServerPartT IO))
type RedirectAdminM = ClckT RedirectAdminURL (ReaderT RedirectConfig (ServerPartT IO))


runRedirectT :: RedirectConfig -> RedirectT m a -> ClckT RedirectURL m a
runRedirectT mc m = mapClckT f m
    where
      f r = runReaderT r mc

runRedirectT'' :: Monad m =>
               (RedirectURL -> [(T.Text, Maybe T.Text)] -> T.Text)
            -> RedirectConfig
            -> RedirectT m a
            -> ClckT url m a
runRedirectT'' showRedirectURL stripeConfig m = ClckT $ withRouteT flattenURL $ unClckT $ runRedirectT stripeConfig $ m
    where
      flattenURL ::   ((url' -> [(T.Text, Maybe T.Text)] -> T.Text) -> (RedirectURL -> [(T.Text, Maybe T.Text)] -> T.Text))
      flattenURL _ u p = showRedirectURL u p


-- withRouteClckT ?
flattenURLClckT :: (url1 -> [(T.Text, Maybe T.Text)] -> T.Text)
                -> ClckT url1 m a
                -> ClckT url2 m a
flattenURLClckT showClckURL m = ClckT $ withRouteT flattenURL $ unClckT m
    where
      flattenURL _ = \u p -> showClckURL u p

clckT2RedirectT :: (Functor m, MonadIO m, MonadFail m, Typeable url1) =>
             ClckT url1 m a
          -> RedirectT m a
clckT2RedirectT m =
    do p <- plugins <$> get
       (Just clckShowFn) <- getPluginRouteFn p (pluginName clckPlugin)
       flattenURLClckT clckShowFn $ mapClckT addReaderT m
    where
      addReaderT :: (Monad m) => m (a, ClckState) -> ReaderT RedirectConfig m (a, ClckState)
      addReaderT m =
          do (a, cs) <- lift m
             return (a, cs)

data RedirectFormError
    = RedirectCFE (CommonFormError [Input])
    | RedirectErrorInternal
      deriving Show

instance FormError RedirectFormError where
    type ErrorInputType RedirectFormError = [Input]
    commonFormError = RedirectCFE

instance (Functor m, Monad m) => EmbedAsChild (RedirectT m) RedirectFormError where
    asChild e = asChild (show e)

type RedirectForm = ClckFormT RedirectFormError RedirectM

instance (Monad m) => MonadReader RedirectConfig (RedirectT' url m) where
    ask = ClckT $ ask
    local f (ClckT m) = ClckT $ local f m

instance (Functor m, Monad m) => GetAcidState (RedirectT' url m) RedirectState where
    getAcidState =
        redirectState <$> ask

instance (IsName n TL.Text) => EmbedAsAttr RedirectM (Attr n RedirectURL) where
        asAttr (n := u) =
            do url <- showURL u
               asAttr $ MkAttr (toName n, pAttrVal (TL.fromStrict url))

instance (IsName n TL.Text) => EmbedAsAttr RedirectM (Attr n ClckURL) where
        asAttr (n := url) =
            do showFn <- redirectClckURL <$> ask
               asAttr $ MkAttr (toName n, pAttrVal (TL.fromStrict $ showFn url []))

{-
-- | convert 'Markup' to 'Content' that can be embedded. Generally by running the pre-processors needed.
-- markupToContent :: (Functor m, MonadIO m, Happstack m) => Markup -> ClckT url m Content
markupToContent :: (Functor m, MonadIO m, Happstack m) =>
                   Markup
                -> ClckT url m Content
markupToContent Markup{..} =
    do clckState <- get
       transformers <- getPreProcessors (plugins clckState)
       (Just clckRouteFn) <- getPluginRouteFn (plugins clckState) (pluginName clckPlugin)
       (markup', clckState') <- liftIO $ runClckT clckRouteFn clckState (foldM (\txt pp -> pp txt) (TL.fromStrict markup) transformers)
       put clckState'
       e <- liftIO $ runPreProcessors preProcessors trust (TL.toStrict markup')
       case e of
         (Left err)   -> return (PlainText err)
         (Right html) -> return (TrustedHtml html)

{-
-- | update the 'currentRedirect' field of 'ClckState'
setCurrentRedirect :: (MonadIO m) => RedirectId -> RedirectT m ()
setCurrentRedirect pid =
    modify $ \s -> s { pageCurrent = pid }
-}
-}
