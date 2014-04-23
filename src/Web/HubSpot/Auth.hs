module Web.HubSpot.Auth where

--------------------------------------------------------------------------------

import Web.HubSpot.Common
import Web.HubSpot.Internal
import qualified Data.ByteString as BS

--------------------------------------------------------------------------------

-- | This is the HubSpot authentication URL to be requested with a GET. Use
-- 'parseAuthQuery' to parse the query string included in the redirect URL.
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

-- | Use this on the query received at the redirect URL given to 'makeAuthUrl'.
--
-- If authentication was successful, the 'Right' result is the 'Auth'.
--
-- If authentication failed, the 'Left' result is an error message.
parseAuthQuery :: Query -> Either ByteString Auth
parseAuthQuery q =
  maybe (Left $ fromMaybe err $ lookupQ "error" q) Right $ do
    access_token <- lookupQ "access_token" q
    expires_in <- join $ intFromBS <$> lookupQ "expires_in" q
    return $ Auth
      access_token
      (lookupQ "refresh_token" q)
      (Left expires_in)
  where
    err = "parseAuthQuery: failed to parse query: " <> renderQuery False q

-- | Do the same as 'parseAuthQuery' but calculate the actual expiration time
-- using the current time and the seconds provided by HubSpot.
parseAuthQueryTime :: MonadIO m => Query -> m (Either ByteString Auth)
parseAuthQueryTime q = do
  t <- liftIO getCurrentTime
  let updateTime at =
        let Left seconds = authExpiresIn at
        in  at { authExpiresIn = Right $ addUTCTime (fromIntegral seconds) t }
  return $ updateTime <$> parseAuthQuery q

-- | Refresh the access token.
refreshTokens
  :: MonadIO m
  => Auth
  -> ClientId
  -> Manager
  -> m (Either String (Auth, PortalId))
refreshTokens tok clientId mgr =
  case authRefreshToken tok of
    Nothing -> return $ Left "refreshTokens: No refresh_token provided"
    Just refreshToken -> do
      req <- liftIO (parseUrl "https://api.hubapi.com/auth/v1/refresh") >>=
        acceptJSON >>=
        formDataBody [ partBS "refresh_token" refreshToken
                     , partBS "client_id" $ fromClientId clientId
                     , partBS "grant_type" "refresh_token"
                     ]
      rsp <- httpLbs req mgr
      case statusCode $ responseStatus rsp of
        200 -> liftM Right $
          (,) `liftM` jsonContent "refreshTokens: authTokens" rsp
              `ap`    jsonContent "refreshTokens: portalId" rsp
        401 -> return $ Left "refreshTokens: Unauthorized request"
        410 -> return $ Left "refreshTokens: Requested an inactive portal"
        500 -> return $ Left "refreshTokens: HubSpot server error"
        c   -> fail $ "refreshTokens: Unsupported HTTP status: " ++ show c
