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

-- | OAuth access token
--
-- Note: Use the 'IsString' instance (e.g. with @OverloadedStrings@) for easy
-- construction.
newtype AccessToken = AccessToken { fromAccessToken :: ByteString }
  deriving (IsString)

instance Show AccessToken where
  show = showBS . fromAccessToken

--------------------------------------------------------------------------------

-- | Hub ID (sometimes called portal ID or account number)
--
-- Note: Use the 'Num' instance for easy construction.
newtype HubId = HubId { fromHubId :: Int }
  deriving (Num)

instance Read HubId where
  readsPrec n = map (first HubId) . readsPrec n

instance Show HubId where
  show = show . fromHubId

hubIdQueryVal :: HubId -> ByteString
hubIdQueryVal = intToBS . fromHubId

--------------------------------------------------------------------------------

-- | An OAuth scope from https://developers.hubspot.com/auth/oauth_scopes
type Scope = ByteString
