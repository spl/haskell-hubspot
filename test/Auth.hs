module Auth where

import           Control.Applicative
import           Data.IORef
import qualified Data.Text          as TS
import qualified Data.Text.Encoding as TS
import qualified Data.Text.Read     as TS
import           Network.Wai
import qualified Web.HubSpot as HS
import           Yesod.Core

import Foundation

postAuthR :: Handler ()
postAuthR = do
  App{..} <- getYesod
  render :: Route App -> TS.Text <- getUrlRender

  clientId <- lookupPostParam "clientId" >>= \case
    Nothing -> invalidArgs ["Missing clientId"]
    Just t  -> return $ HS.ClientId $ TS.encodeUtf8 t
  liftIO $ writeIORef clientIdRef $ Just clientId

  portalId <- lookupPostParam "portalId" >>= \case
    Nothing -> invalidArgs ["Missing portalId"]
    Just t  -> case TS.decimal t of
      Right (n, "") -> return $ HS.PortalId n
      _ -> invalidArgs ["Bad format for portalId (must be an integer)"]
  liftIO $ writeIORef portalIdRef $ Just portalId

  let url = HS.makeAuthUrl clientId portalId (TS.encodeUtf8 $ render AuthR) ["contacts-rw", "offline"]
  redirect $ TS.decodeUtf8 url

getAuthR :: Handler ()
getAuthR = do
  query <- queryString <$> waiRequest
  eAuth <- HS.parseAuth query
  auth <- either (invalidArgs . pure . TS.decodeUtf8) return eAuth
  App{..} <- getYesod
  liftIO $ writeIORef authRef $ Just auth
  redirect HomeR
