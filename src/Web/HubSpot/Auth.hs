module Web.HubSpot.Auth
  ( makeAuthUrl
  , parseAuth
  , refreshAuth
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
      [ ( "client_id"    , Just $ fromClientId clientId     )
      , ( "portalId"     , Just $ portalIdQueryVal portalId )
      , ( "redirect_uri" , Just $ redirectUrl               )
      ]
    -- Scopes are not rendered as above because that would
    -- percent-encode the "+".
  , "&scope="
  , BS.intercalate "+" $ map (urlEncode True) scopes
  ]

-- | Parse the query provided by HubSpot via the redirect URL given to
-- 'makeAuthUrl'. If authentication was successful, the 'Right' result contains
-- the access token, optional refresh token, and expiration time of the access
-- token. If authentication failed, the 'Left' result contains an error message.
parseAuth :: MonadIO m => Query -> m (Either ByteString Auth)
parseAuth q = sequence $ mkAuth <$> parseAuthSimple q

-- | Do the same as 'parseAuth', but provide the number of seconds until
-- expiration without calculating the actual expiration time.
parseAuthSimple :: Query -> Either ByteString (ByteString, Maybe ByteString, Int)
parseAuthSimple q =
  maybe (Left $ fromMaybe err $ lookupQ "error" q) Right $ do
    access_token <- lookupQ "access_token" q
    expires_in <- join $ intFromBS <$> lookupQ "expires_in" q
    return (access_token, lookupQ "refresh_token" q, expires_in)
  where
    err = "parseAuthSimple: failed to parse query: " <> renderQuery False q

-- | Refresh the access token.
refreshAuth
  :: MonadIO m
  => Auth
  -> ClientId
  -> Manager
  -> m (Either String (Auth, PortalId))
refreshAuth Auth {..} clientId mgr =
  case authRefreshToken of
    Nothing -> return $ Left "refreshAuth: No refresh_token provided"
    Just refreshToken -> do
      req <- parseUrl "https://api.hubapi.com/auth/v1/refresh" >>=
        acceptJSON >>=
        setUrlEncodedBody [ ( "refresh_token" , refreshToken          )
                          , ( "client_id"     , fromClientId clientId )
                          , ( "grant_type"    , "refresh_token"       )
                          ]
      rsp <- httpLbs req mgr
      case statusCode $ responseStatus rsp of
        200 -> liftM Right $
          (,) `liftM` mkAuthFromResponse rsp
              `ap`    jsonContent "refreshAuth: portalId" rsp
        401 -> return $ Left "refreshAuth: Unauthorized request"
        410 -> return $ Left "refreshAuth: Requested an inactive portal"
        500 -> return $ Left "refreshAuth: HubSpot server error"
        c   -> fail $ "refreshAuth: Unsupported HTTP status: " ++ show c
