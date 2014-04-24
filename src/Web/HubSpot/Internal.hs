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

-- | Error message JSON object returned by HubSpot
data ErrorMessage = ErrorMessage
  { errorMessage   :: !Text
  , errorRequestId :: !Text
  }
  deriving Show

instance ToJSON ErrorMessage where
  toJSON ErrorMessage {..} = object
    [ "status"    .= String "error"
    , "message"   .= String errorMessage
    , "requestId" .= String errorRequestId
    ]

instance FromJSON ErrorMessage where
  parseJSON = withObject "ErrorMessage" $ \o -> do
    ErrorMessage <$> o .: "message"
                 <*> o .: "requestId"

--------------------------------------------------------------------------------

-- | This represents a contact property (or field) object.
--
-- Ideally, we would use only enumerations for the values of 'cpType' and
-- 'cpFieldType'. However, we have observed unexpected strings from HubSpot, and
-- we use 'Left' for these values.
--
-- https://developers.hubspot.com/docs/methods/contacts/create_property
data ContactProperty = ContactProperty
  { cpName          :: !Text
  , cpLabel         :: !Text
  , cpDescription   :: !Text
  , cpGroupName     :: !Text
  , cpType          :: !(Either Text ContactPropertyType)
  , cpFieldType     :: !(Either Text ContactPropertyFieldType)
  , cpFormField     :: !Bool
  , cpDisplayOrder  :: !Int
  , cpOptions       :: ![ContactPropertyOption]
  }
  deriving (Show)

eitherToJSON :: (ToJSON a, ToJSON b) => Either a b -> Value
eitherToJSON = either toJSON toJSON

instance ToJSON ContactProperty where
  toJSON ContactProperty {..} = object
    [ "name"         .= toJSON cpName
    , "label"        .= toJSON cpLabel
    , "description"  .= toJSON cpDescription
    , "groupName"    .= toJSON cpGroupName
    , "type"         .= eitherToJSON cpType
    , "fieldType"    .= eitherToJSON cpFieldType
    , "formField"    .= toJSON cpFormField
    , "displayOrder" .= toJSON cpDisplayOrder
    , "options"      .= toJSON cpOptions
    ]

-- | Parse alternatives: first 'Right', then 'Left'
parseAlt :: (FromJSON a, FromJSON b) => Object -> Text -> Parser (Either a b)
parseAlt o name = Right <$> o .: name <|> Left <$> o .: name

instance FromJSON ContactProperty where
  parseJSON = withObject "ContactProperty" $ \o -> do
    ContactProperty <$> o .: "name"
                    <*> o .: "label"
                    <*> o .: "description"
                    <*> o .: "groupName"
                    <*> parseAlt o "type"
                    <*> parseAlt o "fieldType"
                    <*> o .: "formField"
                    <*> o .: "displayOrder"
                    <*> o .: "options"

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
  deriving (Show)

--------------------------------------------------------------------------------
-- Template Haskell declarations go at the end.

deriveJSON_ ''ContactPropertyType       (defaultEnumOptions   3)
deriveJSON_ ''ContactPropertyFieldType  (defaultEnumOptions   4)

deriveJSON_ ''ContactPropertyOption     (defaultRecordOptions 3)
