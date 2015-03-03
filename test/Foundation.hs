module Foundation where

import           Data.IORef
import qualified Web.HubSpot as HS
import           Yesod.Core

data App = App
  { clientIdRef  :: IORef (Maybe HS.ClientId)
  , portalIdRef  :: IORef (Maybe HS.PortalId)
  , authRef      :: IORef (Maybe HS.Auth)
  }

mkYesodData "App" $(parseRoutesFile "routes")

instance Yesod App where
  approot = ApprootStatic "http://localhost:3000"
