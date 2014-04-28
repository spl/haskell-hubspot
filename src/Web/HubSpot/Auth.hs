module Web.HubSpot.Auth
  ( makeAuthUrl
  , parseAuth
  , refreshAuth
  ) where

--------------------------------------------------------------------------------

import Web.HubSpot.Common
import Web.HubSpot.Internal
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TS

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
  , renderQuery True $ ("redirect_uri", Just redirectUrl') : clientPortalQuery
    -- Scopes are not rendered as above because that would
    -- percent-encode the "+".
  , "&scope="
  , BS.intercalate "+" $ map (urlEncode True) scopes
  ]
  where
    clientPortalQuery =
      [ ( "client_id" , Just clientId             )
      , ( "portalId"  , Just $ intToBS $ portalId )
      ]
    -- Include the client and portal IDs in the redirect URL for 'parseAuth'.
    redirectUrl' = case BS.breakByte 63 {- '?' -} redirectUrl of
      (url, query) -> url <> renderQuery True (clientPortalQuery ++ parseQuery query)

-- | Parse the query provided by HubSpot via the redirect URL given to
-- 'makeAuthUrl'. If authentication was successful, the 'Right' result contains
-- the access token, optional refresh token, and expiration time of the access
-- token. If authentication failed, the 'Left' result contains an error message.
parseAuth :: MonadIO m => Query -> m (Either ByteString Auth)
parseAuth q = do
  let err = "parseAuth: failed to parse query: " <> renderQuery False q
  tm <- liftIO getCurrentTime
  return $ maybe (Left $ fromMaybe err $ lookupQ "error" q) Right $ do
    Auth <$> lookupQ "client_id" q
         <*> join (intFromBS <$> lookupQ "portalId" q)
         <*> lookupQ "access_token" q
         <*> pure (lookupQ "refresh_token" q)
         <*> (expireTime tm <$> join (intFromBS <$> lookupQ "expires_in" q))

-- | Refresh the access token.
refreshAuth
  :: MonadIO m
  => Auth
  -> Manager
  -> m (Either String Auth)
refreshAuth auth@Auth {..} mgr =
  case authRefreshToken of
    Nothing -> return $ Left "refreshAuth: No refresh_token provided"
    Just refreshToken -> do
      req <- parseUrl "https://api.hubapi.com/auth/v1/refresh" >>=
        acceptJSON >>=
        setUrlEncodedBody [ ( "refresh_token" , refreshToken    )
                          , ( "client_id"     , authClientId    )
                          , ( "grant_type"    , "refresh_token" )
                          ]
      rsp <- httpLbs req mgr
      case statusCode $ responseStatus rsp of
        200 -> Right `liftM` (jsonContent "refreshAuth" rsp >>= parseRefreshAuth auth)
        401 -> return $ Left "refreshAuth: Unauthorized request"
        410 -> return $ Left "refreshAuth: Requested an inactive portal"
        500 -> return $ Left "refreshAuth: HubSpot server error"
        c   -> fail $ "refreshAuth: Unsupported HTTP status: " ++ show c

--------------------------------------------------------------------------------

expireTime :: UTCTime -> Int -> UTCTime
expireTime tm sec = fromIntegral sec `addUTCTime` tm

parseRefreshAuth :: MonadIO m => Auth -> Object -> m Auth
parseRefreshAuth Auth {..} obj = do
  tm <- liftIO getCurrentTime
  either (fail . mappend "pAuth") return $ flip parseEither obj $ \o ->
    Auth <$> pure authClientId
         <*> o .: "portal_id"
         <*> (TS.encodeUtf8 <$> o .: "access_token")
         <*> (pure . TS.encodeUtf8 <$> o .: "refresh_token")
         <*> (expireTime tm <$> o .: "expires_in")
