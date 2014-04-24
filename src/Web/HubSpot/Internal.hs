module Web.HubSpot.Internal where

--------------------------------------------------------------------------------

import Web.HubSpot.Common
import qualified Data.ByteString.Lazy as BL
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

-- | Authentication information
data Auth = Auth
  { authAccessToken  :: ByteString       -- ^ Access token
  , authRefreshToken :: Maybe ByteString -- ^ Refresh token for "offline" scope
  , authExpiresIn    :: UTCTime          -- ^ Expiration time of of the access token
  }
  deriving (Show)

instance ToJSON Auth where
  toJSON (Auth {..}) = object
    [ "access_token"  .= (String . TS.decodeUtf8 $ authAccessToken)
    , "refresh_token" .= (String . TS.decodeUtf8 <$> authRefreshToken)
    , "expires_in"    .= toJSON authExpiresIn
    ]

instance FromJSON Auth where
  parseJSON = withObject "Auth" $ \o -> do
    Auth <$> (TS.encodeUtf8 <$> o .: "access_token")
         <*> (fmap TS.encodeUtf8 <$> o .: "refresh_token")
         <*> o .: "expires_in"

expireTime :: UTCTime -> Int -> UTCTime
expireTime tm sec = fromIntegral sec `addUTCTime` tm

mkAuth :: MonadIO m => (ByteString, Maybe ByteString, Int) -> m Auth
mkAuth (at,rt,sec) = do
  tm <- liftIO getCurrentTime
  return $ Auth at rt $ expireTime tm sec

pAuth :: Monad m => UTCTime -> Object -> m Auth
pAuth tm = either (fail . mappend "pAuth") return . parseEither (\o ->
  Auth <$> (TS.encodeUtf8 <$> o .: "access_token")
       <*> (Just . TS.encodeUtf8 <$> o .: "refresh_token")
       <*> (expireTime tm <$> o .: "expires_in"))

mkAuthFromResponse :: MonadIO m => Response BL.ByteString -> m Auth
mkAuthFromResponse rsp = do
  tm <- liftIO getCurrentTime
  jsonContent "Auth" rsp >>= pAuth tm

newAuthReq :: MonadIO m => Auth -> String -> m Request
newAuthReq Auth {..} s = parseUrl s
  >>= setQuery [("access_token", Just authAccessToken)]

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

--------------------------------------------------------------------------------

data ContactProperty = ContactProperty
  { cpName          :: !Text
  , cpLabel         :: !Text
  , cpDescription   :: !Text
  , cpGroupName     :: !Text
  , cpType          :: !ContactPropertyType
  , cpFieldType     :: !ContactPropertyFieldType
  , cpFormField     :: !Bool
  , cpDisplayOrder  :: !Int
  , cpOptions       :: ![ContactPropertyOption]
  }

data ContactPropertyType
  = CPTString
  | CPTNumber
  | CPTBool
  | CPTDateTime
  | CPTEnumeration
  deriving (Eq, Enum, Bounded, Read, Show)

data ContactPropertyFieldType
  = CPFTTextArea
  | CPFTSelect
  | CPFTText
  | CPFTDate
  | CPFTFile
  | CPFTNumber
  | CPFTRadio
  | CPFTCheckBox
  deriving (Eq, Enum, Bounded, Read, Show)

data ContactPropertyOption = ContactPropertyOption
  { cpoLabel        :: !Text
  , cpoValue        :: !Text
  , cpoDisplayOrder :: !Int
  }

--------------------------------------------------------------------------------
-- Template Haskell declarations go at the end.

deriveJSON_ ''ContactPropertyType       (defaultEnumOptions   3)
deriveJSON_ ''ContactPropertyFieldType  (defaultEnumOptions   4)

deriveJSON_ ''ContactPropertyOption     (defaultRecordOptions 3)
deriveJSON_ ''ContactProperty           (defaultRecordOptions 2)
