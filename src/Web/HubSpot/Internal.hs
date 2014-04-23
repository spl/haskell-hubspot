module Web.HubSpot.Internal where

--------------------------------------------------------------------------------

import Web.HubSpot.Common
import qualified Data.Text.Encoding as TS

--------------------------------------------------------------------------------

-- | Client ID
--
-- Note: Use the 'IsString' instance (e.g. with @OverloadedStrings@) for easy
-- construction.
newtype ClientId = ClientId { fromClientId :: ByteString }
  deriving (IsString)

instance Show ClientId where
  show = showBS . fromClientId

--------------------------------------------------------------------------------

-- | The authentication tokens include the access token, optional refresh token,
-- and expiration time of the access token.
data AuthTokens = AuthTokens
  { authAccessToken     :: ByteString
  , authRefreshToken    :: Maybe ByteString -- ^ Provided when using the "offline" scope
  , authExpiresIn       :: Either Int UTCTime -- ^ Number of seconds or time
  }
  deriving (Show)

instance FromJSON AuthTokens where
  parseJSON = withObject "AuthTokens" $ \o -> do
    AuthTokens <$> (TS.encodeUtf8 <$> o .: "access_token")
               <*> (Just . TS.encodeUtf8 <$> o .: "refresh_token")
               <*> (Left <$> o .: "expires_in")

--------------------------------------------------------------------------------

-- | Portal ID (sometimes called Hub ID or account number)
--
-- Note: Use the 'Num' instance for easy construction.
newtype PortalId = PortalId { fromPortalId :: Int }
  deriving (Num)

instance Read PortalId where
  readsPrec n = map (first PortalId) . readsPrec n

instance Show PortalId where
  show = show . fromPortalId

instance FromJSON PortalId where
  parseJSON = withObject "PortalId" $ \o -> do
    PortalId <$> o .: "portal_id"

portalIdQueryVal :: PortalId -> ByteString
portalIdQueryVal = intToBS . fromPortalId

--------------------------------------------------------------------------------

-- | An OAuth scope from https://developers.hubspot.com/auth/oauth_scopes
type Scope = ByteString
