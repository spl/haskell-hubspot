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
parseAuthQuery :: Query -> Either ByteString AuthTokens
parseAuthQuery q =
  maybe (Left $ fromMaybe err $ lookupQ "error" q) Right $ do
    access_token <- lookupQ "access_token" q
    expires_in <- join $ intFromBS <$> lookupQ "expires_in" q
    return $ AuthTokens
      access_token
      (lookupQ "refresh_token" q)
      (Left expires_in)
  where
    err = "parseAuthQuery: failed to parse query: " <> renderQuery False q

-- | Do the same as 'parseAuthQuery' but calculate the actual expiration time
-- using the current time.
parseAuthQueryTime :: MonadIO m => Query -> m (Either ByteString AuthTokens)
parseAuthQueryTime q = do
  t <- liftIO getCurrentTime
  let updateTime at =
        let Left seconds = authExpiresIn at
        in  at { authExpiresIn = Right $ addUTCTime (fromIntegral seconds) t }
  return $ updateTime <$> parseAuthQuery q

-- | Refresh the access token.
refreshTokens
  :: MonadIO m
  => AuthTokens
  -> ClientId
  -> Manager
  -> m (Either String (AuthTokens, PortalId))
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
