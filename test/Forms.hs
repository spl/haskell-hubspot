module Forms where

import qualified Data.Aeson as A
import           Data.IORef (readIORef)
import qualified Data.Text as TS
import           Network.HTTP.Client (withManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Text.Blaze as Blaze
import qualified Web.HubSpot as HS
import           Yesod.Core

import           Foundation

getFormsAllR :: Handler Html
getFormsAllR = do
  App{..} <- getYesod
  auth <- maybe (invalidArgs ["Not authenticated"]) return =<< liftIO (readIORef authRef)
  forms <- liftIO $ withManager tlsManagerSettings $ HS.getAllForms auth
  defaultLayout $ do
    let title = "HubSpot Test: Forms"
    setTitle title
    [whamlet|
      <h1>#{title}
      <p>
        <ol>
        $forall form <- forms
          <li>
            <code>
              #{Blaze.unsafeLazyByteString $ A.encode form}
          <br>
    |]

getFormR :: String -> Handler Html
getFormR formId = do
  App{..} <- getYesod
  auth <- maybe (invalidArgs ["Not authenticated"]) return =<< liftIO (readIORef authRef)
  form <- liftIO $ withManager tlsManagerSettings $ HS.getForm (TS.pack formId) auth
  let title = Blaze.string $ "HubSpot Test: Form " ++ formId
  defaultLayout $ do
    setTitle title
    [whamlet|
      <h1>#{title}
        <code>
          #{Blaze.string formId}
      <p>
        <code>
          #{Blaze.unsafeLazyByteString $ A.encode form}
    |]
