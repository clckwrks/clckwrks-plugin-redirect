{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}
module Clckwrks.Redirect.URL where

import Data.Data (Data, Typeable)
import Data.SafeCopy               (SafeCopy(..), base, deriveSafeCopy)
import Clckwrks.Redirect.Acid      ()
import Clckwrks.Redirect.Types     ()
import Web.Routes.TH               (derivePathInfo)

data RedirectAdminURL
    = EditRedirects
      deriving (Eq, Ord, Data, Typeable, Read, Show)
$(deriveSafeCopy 0 'base ''RedirectAdminURL)
$(derivePathInfo ''RedirectAdminURL)

data RedirectURL
    = Redirect
      deriving (Eq, Ord, Data, Typeable, Read, Show)
$(deriveSafeCopy 0 'base ''RedirectURL)
$(derivePathInfo ''RedirectURL)
