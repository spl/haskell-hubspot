module Web.HubSpot.Authentication where

--------------------------------------------------------------------------------

import Web.HubSpot.Common
import Web.HubSpot.Internal
import qualified Data.ByteString as BS

--------------------------------------------------------------------------------

-- | Authenticate with HubSpot using a redirect URL that is accessed with a GET
-- and includes a query with access_token and expires_in. Since the access_token
-- is included, the URL should be secure (https) to avoid leaking the token.
authenticate
  :: MonadIO m
  => ClientId
  -> HubId
  -> ByteString  -- ^ Redirect URL
  -> [Scope]
  -> Manager
  -> m ()
authenticate clientId hubId redirectUrl scopes mgr = do
  req <- liftIO (parseUrl "https://app.hubspot.com/auth/authenticate") >>=
    setQuery [ ( "client_id"    , Just $ fromClientId clientId     )
             , ( "portalId"     , Just $ hubIdQueryVal hubId       )
             , ( "redirect_uri" , Just $ redirectUrl               )
             , ( "scope"        , Just $ BS.intercalate "+" scopes )
             ]
  _ <- httpLbs req mgr
  return ()

-- | Use this on the query received at the redirect URL given to 'authenticate'.
fromAuthenticateUrl :: Query -> Maybe (AccessToken, Int)
fromAuthenticateUrl query =
  (,) <$> AccessToken <$> lookupQ "access_token" query
      <*> join (intFromBS <$> lookupQ "expires_in" query)
