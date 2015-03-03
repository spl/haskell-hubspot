module Home where

import qualified Data.Aeson as A
import           Data.IORef (readIORef)
import qualified Text.Blaze as Blaze
import           Yesod.Core

import           Foundation

getHomeR :: Handler Html
getHomeR = do
  App{..} <- getYesod
  mPortalId <- liftIO $ readIORef portalIdRef
  mClientId <- liftIO $ readIORef clientIdRef
  mAuth     <- liftIO $ readIORef authRef
  defaultLayout $ do
    let title = "HubSpot Test"
    setTitle title
    [whamlet|
      <h1>#{title}
      <h2>Authenticate
      <p>
        <form method="POST" action=@{AuthR}>
          <label>Portal ID
          $maybe portalId <- mPortalId
            <input type="text" name="portalId" value="#{Blaze.string $ show portalId}">
          $nothing
            <input type="text" name="portalId">
          <br>
          <label>Client ID
          $maybe clientId <- mClientId
            <input type="text" name="clientId" value="#{Blaze.string $ show clientId}">
          $nothing
            <input type="text" name="clientId">
          <br>
          <input type="submit">
      $maybe auth <- mAuth
        <h2>Authenticated
        <p>
          Tokens:
          <code>
            #{Blaze.unsafeLazyByteString $ A.encode auth}
        <h2>Forms
        <p>
          <a href=@{FormsAllR}>Get all forms
        <p>
          <form method="GET" action=@{FormR "REPLACEME"}>
            <label>Form ID
            <input type="text" name="formId">
            <input type="submit" onclick="this.form.action=this.form.action.replace('REPLACEME', this.form.formId.value);">
    |]
