{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Web.HubSpot.Auth
  ( makeAuthUrl
  , parseAuth
  , refreshAuth
  , withRefresh
  , checkRefreshAuth
  ) where

--------------------------------------------------------------------------------

import Web.HubSpot.Common
import Web.HubSpot.Internal
import qualified Data.ByteString as BS

--------------------------------------------------------------------------------

-- | This is the HubSpot authentication URL to be requested with a GET. Use
-- 'parseAuth' to parse the query string included in the redirect URL.
--
-- Note: Since the access_token is included, the redirect URL should be secure
-- (https) to avoid leaking the token.
makeAuthUrl
  :: ClientId
  -> PortalId
  -> ByteString  -- ^ Redirect URL
  -> [Scope]
  -> ByteString
makeAuthUrl clientId portalId redirectUrl scopes = mconcat
  [ "https://app.hubspot.com/auth/authenticate"
  , renderQuery True
      [ ("redirect_uri" , Just redirectUrl                       )
      , ( "client_id"   , Just $ fromClientId clientId           )
      , ( "portalId"    , Just $ intToBS $ fromPortalId portalId )
      ]
  , "&scope="
    -- Do not use renderQuery for scopes to avoid percent-encoding the "+".
  , BS.intercalate "+" $ map (urlEncode True . fromScope) scopes
  ]

-- | Parse the query provided by HubSpot via the redirect URL given to
-- 'makeAuthUrl'. If authentication was successful, the 'Right' result contains
-- the access token, optional refresh token, and expiration time of the access
-- token. If authentication failed, the 'Left' result contains an error message.
parseAuth :: MonadIO m => Query -> m (Either ByteString Auth)
parseAuth q = do
  let err = "parseAuth: failed to parse query: " <> renderQuery False q
  tm <- liftIO getCurrentTime
  return $ maybe (Left $ fromMaybe err $ lookupQ "error" q) Right $ do
    Auth <$> (AccessToken <$> lookupQ "access_token" q)
         <*> (Just . RefreshToken <$> lookupQ "refresh_token" q)
         <*> (expireTime tm <$> join (intFromBS <$> lookupQ "expires_in" q))

-- | Refresh the access token
--
-- Note: If the 'Auth' argument does not have a refresh token, 'refreshAuth'
-- will throw a 'NoRefreshToken' exception.
refreshAuth
  :: MonadIO m
  => ClientId
  -> Auth
  -> Manager
  -> m Auth
refreshAuth clientId Auth {..} mgr =
  case authRefreshToken of
    Nothing ->
      -- We throw an exception in this case because it is simple. A more
      -- type-safe approach would be to change 'Auth' to indicate whether it
      -- includes a 'RefreshToken', and 'refreshAuth' could only be called when
      -- it does. However, that would complicate types for what we expect is a
      -- rare occurrence (calling 'refreshAuth' with an 'Auth' which doesn't
      -- have a 'RefreshToken').
      liftIO $ throwIO NoRefreshToken
    Just refreshToken -> do
      parseUrl "https://api.hubapi.com/auth/v1/refresh"
      >>= acceptJSON
      >>= setUrlEncodedBody [ ( "refresh_token" , fromRefreshToken refreshToken )
                            , ( "client_id"     , fromClientId clientId )
                            , ( "grant_type"    , "refresh_token"       )
                            ]
      >>= flip httpLbs mgr
      >>= checkResponse
      >>= fromJSONResponse "refreshAuth"
      >>= parseRefreshAuth

-- | Given a function that requires authentication, first check if the access
-- token has expired and, if needed, refresh the token. Then, run the function
-- argument. Return the new 'Auth' if available along with the result of the
-- argument.
withRefresh
  :: MonadIO m
  => (Auth -> Manager -> m b)
  -> ClientId
  -> Auth
  -> Manager
  -> m (Maybe Auth, b)
withRefresh run clientId auth@Auth {..} mgr = do
  mauth <- checkRefreshAuth clientId mgr auth
  result <- run (fromMaybe auth mauth) mgr
  return (mauth, result)

checkRefreshAuth :: MonadIO m => ClientId -> Manager -> Auth -> m (Maybe Auth)
checkRefreshAuth clientId mgr auth@Auth {..} = do
  tm <- liftIO getCurrentTime
  let expired = authExpiration `diffUTCTime` tm < 5 * 60 {- 5 minutes -}
  if expired then Just `liftM` refreshAuth clientId auth mgr else return Nothing

--------------------------------------------------------------------------------

expireTime :: UTCTime -> Int -> UTCTime
expireTime tm sec = fromIntegral sec `addUTCTime` tm

parseRefreshAuth :: MonadIO m => Object -> m Auth
parseRefreshAuth obj = do
  tm <- liftIO getCurrentTime
  either (fail . mappend "pAuth") return $ flip parseEither obj $ \o ->
    Auth <$> (o .: "access_token")
         <*> (Just <$> o .: "refresh_token")
         <*> (expireTime tm <$> o .: "expires_in")
