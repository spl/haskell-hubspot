module Web.HubSpot.Internal where

--------------------------------------------------------------------------------

import Web.HubSpot.Common

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
  , authExpirationTime  :: UTCTime
  }
  deriving (Show)

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

portalIdQueryVal :: PortalId -> ByteString
portalIdQueryVal = intToBS . fromPortalId

--------------------------------------------------------------------------------

-- | An OAuth scope from https://developers.hubspot.com/auth/oauth_scopes
type Scope = ByteString
