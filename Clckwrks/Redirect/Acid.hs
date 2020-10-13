{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies, RecordWildCards, OverloadedStrings, QuasiQuotes #-}
module Clckwrks.Redirect.Acid
    ( -- * state
      RedirectState
    , initialRedirectState
      -- * events
    , GetRedirect(..)
--    , SetRedirect(..)
    ) where

import Clckwrks                (UserId(..))
-- import Clckwrks.Redirect.Types ()
import Control.Applicative     ((<$>))
import Control.Monad.Reader    (ask)
import Control.Monad.State     (get, modify, put)
import Control.Monad.Trans     (liftIO)
import Data.Acid               (AcidState, Query, Update, makeAcidic)
import Data.Data               (Data, Typeable)
import Data.IxSet              (Indexable, IxSet, (@=), Proxy(..), empty, fromList, getOne, ixSet, ixFun, insert, toList, toDescList, updateIx)
import           Data.Map      (Map)
import qualified Data.Map      as Map
import Data.Maybe              (fromJust)
import Data.SafeCopy           (Migrate(..), base, deriveSafeCopy, extension)
import Data.String             (fromString)
import Data.Text               (Text)
import qualified Data.Text     as Text
-- import           Data.UUID     (UUID)
-- import qualified Data.UUID     as UUID
-- import HSP.Google.Analytics (UACCT)

data RedirectState = RedirectState
    { redirects  :: Map [Text] Text
    }
    deriving (Eq, Read, Show, Data, Typeable)
deriveSafeCopy 0 'base ''RedirectState

initialRedirectState :: IO RedirectState
initialRedirectState =
  pure $ RedirectState { redirects = Map.empty }

getRedirect :: [Text]
            -> Query RedirectState (Maybe Text)
getRedirect paths =
  do s <- ask
     pure $ Map.lookup paths (redirects s)

makeAcidic ''RedirectState
  [ 'getRedirect
  ]
