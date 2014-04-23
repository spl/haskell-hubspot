module Web.HubSpot.Authentication where

--------------------------------------------------------------------------------

import Web.HubSpot.Common
import Web.HubSpot.Internal
import qualified Data.ByteString as BS

--------------------------------------------------------------------------------

-- | Authenticate with HubSpot using a redirect URL that is accessed with a GET
-- and includes a query with access_token and expires_in. Since the access_token
-- is included, the redirect URL should be secure (https) to avoid leaking the
-- token.
authenticate
  :: MonadIO m
  => ClientId
  -> PortalId
  -> ByteString  -- ^ Redirect URL
  -> [Scope]
  -> Manager
  -> m ()
authenticate clientId portalId redirectUrl scopes mgr = do
  req <- liftIO (parseUrl "https://app.hubspot.com/auth/authenticate") >>=
    setQuery [ ( "client_id"    , Just $ fromClientId clientId     )
             , ( "portalId"     , Just $ portalIdQueryVal portalId )
             , ( "redirect_uri" , Just $ redirectUrl               )
             , ( "scope"        , Just $ BS.intercalate "+" scopes )
             ]
  _ <- httpLbs req mgr
  return ()

-- | Use this on the query received at the redirect URL given to 'authenticate'.
--
-- If authentication was successful, the 'Right' result is the 'AuthTokens'.
--
-- If authentication failed, the 'Left' result is an error message.
parseAuthenticateQuery :: MonadIO m => Query -> m (Either ByteString AuthTokens)
parseAuthenticateQuery q = do
  t <- liftIO getCurrentTime
  return $ maybe (Left $ fromMaybe err $ lookupQ "error" q) Right $ do
    access_token <- lookupQ "access_token" q
    expires_in <- join $ intFromBS <$> lookupQ "expires_in" q
    return $ AuthTokens
      access_token
      (lookupQ "refresh_token" q)
      (addUTCTime (fromIntegral expires_in) t)
  where
    err = "parseAuthenticateQuery: failed to parse query: " <> renderQuery False q
