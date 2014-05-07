module Web.HubSpot.Internal where

--------------------------------------------------------------------------------

import Web.HubSpot.Common
import qualified Data.ByteString.Lazy as BL
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TS
import Data.Typeable (Typeable)

--------------------------------------------------------------------------------

-- | This function contains the general flow of a function requesting something
-- from HubSpot and processing the response. It handles setting the acccess
-- token in the query, setting the appropriate Accept type, and checking the
-- HTTP status code in the response.
generalRequest
  :: MonadIO m
  => [Text]
  -> (Request -> IO Request)
  -> (Response BL.ByteString -> IO a)
  -> Auth
  -> Manager
  -> m a
generalRequest urlSegments modifyRequest processResponse Auth {..} mgr =
  liftIO $ handle handleHttpException $
  parseUrl (TS.unpack $ TS.intercalate "/" urlSegments)
  >>= acceptJSON
  >>= setQuery [("access_token", Just authAccessToken)]
  >>= modifyRequest
  >>= flip httpLbs mgr
  >>= checkResponse
  >>= processResponse

-- | Check the response HTTP status codes for successful requests.
--
-- Note: This does not check unsuccessful requests, because 'httpLbs' throws an
-- 'HttpException' for those.
checkResponse :: MonadIO m => Response BL.ByteString -> m (Response BL.ByteString)
checkResponse rsp = do
  let status = responseStatus rsp
      body = responseBody rsp
  case statusCode status of
    200 -> return rsp
    204 -> return rsp
    _   -> throwIO $ UnexpectedHttpResponse status body

handleHttpException :: HttpException -> IO a
handleHttpException e@(StatusCodeException status rspHdrs _) =
  case statusCode status of
    401 -> throwIO $ UnauthorizedRequest mErrMsg
    404 -> throwIO $ DataNotFound mErrMsg
    409 -> throwIO $ ConflictingEdit mErrMsg
    _   -> throwIO e
  where
    mErrMsg = do
      body <- lookup "X-Response-Body-Start" rspHdrs
      decodeStrict' body
handleHttpException e = throwIO e

--------------------------------------------------------------------------------

-- | Exceptions thrown by functions in this package.
data HubSpotException

    -- | A refresh was attempted, but no refresh token was provided.
  = NoRefreshToken

    -- | HTTP Access Code 401 (Unauthorized) which indicates when an Access
    -- Token has expired. The response body is included.
    --
    -- See https://developers.hubspot.com/auth/oauth_apps
  | UnauthorizedRequest (Maybe ErrorMessage)

    -- | HTTP Access Code 404 (Not Found) which indicates that the requested
    -- data is not available.
  | DataNotFound (Maybe ErrorMessage)

    -- | HTTP Access Code 409 (Conflict) which indicates that an edit request
    -- conflicts with current data on the server. This can happen with trying to
    -- create a new entity but an entity with the same name already exists.
  | ConflictingEdit (Maybe ErrorMessage)

    -- | An unexpected HTTP response status with the response body
  | UnexpectedHttpResponse Status BL.ByteString

  deriving (Show, Typeable)

instance Exception HubSpotException

--------------------------------------------------------------------------------

-- | Client ID
--
-- Note: Use the 'IsString' instance (e.g. with @OverloadedStrings@) for easy
-- construction.
newtype ClientId = ClientId { fromClientId :: ByteString }
  deriving IsString

instance Show ClientId where
  show = showBS . fromClientId

instance ToJSON ClientId where
  toJSON = String . TS.decodeUtf8 . fromClientId

instance FromJSON ClientId where
  parseJSON = fmap (ClientId . TS.encodeUtf8) . parseJSON

--------------------------------------------------------------------------------

-- | Portal ID (also called Hub ID or account number)
--
-- Note: Use the 'Num' instance for easy construction.
newtype PortalId = PortalId { fromPortalId :: Int }
  deriving Num

instance Read PortalId where
  readsPrec n = map (first PortalId) . readsPrec n

instance Show PortalId where
  show = show . fromPortalId

instance ToJSON PortalId where
  toJSON = toJSON . fromPortalId

instance FromJSON PortalId where
  parseJSON = fmap PortalId . parseJSON

--------------------------------------------------------------------------------

-- | Access token
type AccessToken = ByteString

-- | Refresh token
type RefreshToken = ByteString

-- | An OAuth scope from https://developers.hubspot.com/auth/oauth_scopes
type Scope = ByteString

--------------------------------------------------------------------------------

-- | Authentication information
data Auth = Auth
  { authAccessToken  :: !AccessToken          -- ^ Access token for OAuth
  , authRefreshToken :: !(Maybe RefreshToken) -- ^ Only for "offline" scope
  , authExpiration   :: !UTCTime              -- ^ Expiration time of the access token
  }
  deriving Show

instance ToJSON Auth where
  toJSON Auth {..} = object
    [ "access_token"  .= TS.decodeUtf8 authAccessToken
    , "refresh_token" .= fmap TS.decodeUtf8 authRefreshToken
    , "expiration"    .= authExpiration
    ]

instance FromJSON Auth where
  parseJSON = withObject "Auth" $ \o -> do
    Auth <$> (TS.encodeUtf8 <$> o .: "access_token")
         <*> (fmap TS.encodeUtf8 <$> o .: "refresh_token")
         <*> o .: "expiration"

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

-- | The standard HubSpot timestamp is the number of milliseconds since the Unix
-- epoch.
newtype Timestamp = TS { fromTS :: POSIXTime }
  deriving (Eq, Ord, Fractional, Real, RealFrac)

-- | Convert to/from an integer representing milliseconds
fromMilliseconds :: Integer -> Timestamp
fromMilliseconds = TS . (/ 1000) . realToFrac

toMilliseconds :: Timestamp -> Integer
toMilliseconds = floor . (* 1000) . fromTS

instance Num Timestamp where
  (+) = TS .$ (+) `on` fromTS
  (*) = TS .$ (*) `on` fromTS
  (-) = TS .$ (-) `on` fromTS
  negate = TS . negate . fromTS
  abs = TS . abs . fromTS
  signum = TS . signum . fromTS
  fromInteger = fromMilliseconds

instance Show Timestamp where
  show = show . fromTS

-- | Note that this instance uses scientific notation (with exponents) while
-- HubSpot's output uses decimal notation (without exponents).
instance ToJSON Timestamp where
  toJSON = toJSON . toMilliseconds

instance FromJSON Timestamp where
  parseJSON = fmap fromMilliseconds . parseJSON

--------------------------------------------------------------------------------

-- | Contact ID (sometimes called visitor ID or vid)
--
-- Note: Use the 'Num' instance for easy construction.
newtype ContactId = ContactId { fromContactId :: Int }
  deriving (Eq, Num, Hashable)

instance Read ContactId where
  readsPrec n = map (first ContactId) . readsPrec n

instance Show ContactId where
  show = show . fromContactId

instance ToJSON ContactId where
  toJSON = toJSON . fromContactId

instance FromJSON ContactId where
  parseJSON = fmap ContactId . parseJSON

--------------------------------------------------------------------------------

-- | Contact User Token (sometimes called HubSpot cookie or hubspotutk)
--
-- Note: Use the 'IsString' instance (e.g. with @OverloadedStrings@) for easy
-- construction.
newtype UserToken = UserToken { fromUserToken :: ByteString }
  deriving IsString

instance Show UserToken where
  show = showBS . fromUserToken

--------------------------------------------------------------------------------

-- | A contact profile
--
-- Note: We currently only decode part of the contact profile. For the rest of
-- the fields, see 'contactObject'.
data Contact = Contact
  { contactId         :: !ContactId
  , contactProperties :: !PropMap
  , contactObject     :: !Object  -- ^ The entire object
  }
  deriving Show

instance ToJSON Contact where
  toJSON Contact {..} = Object contactObject

instance FromJSON Contact where
  parseJSON = withObject "Contact" $ \o -> do
    Contact <$> o .:  "vid"
            <*> o .:  "properties"
            <*> return o

-- | An intermediate type used only for its 'FromJSON' instance.
data ContactPage = ContactPage
  { contacts   :: ![Contact]
  , hasMore    :: !Bool
  , vidOffset  :: !Int
  }
  deriving Show

fromContactPage :: ContactPage -> ([Contact], Bool, Int)
fromContactPage (ContactPage a b c) = (a, b, c)

--------------------------------------------------------------------------------

-- | This represents a contact property (or field) object.
--
-- Ideally, we would use only enumerations for the values of 'propType' and
-- 'propFieldType'. However, we have observed unexpected strings from HubSpot, and
-- we use 'Left' for these values.
--
-- https://developers.hubspot.com/docs/methods/contacts/create_property
data Property = Property
  { propName          :: !Text
  , propLabel         :: !Text
  , propDescription   :: !Text
  , propGroupName     :: !Text
  , propType          :: !(Either Text PropType)
  , propFieldType     :: !(Either Text PropFieldType)
  , propFormField     :: !Bool
  , propDisplayOrder  :: !Int
  , propOptions       :: ![PropOption]
  }
  deriving Show

instance ToJSON Property where
  toJSON Property {..} = object
    [ "name"         .= propName
    , "label"        .= propLabel
    , "description"  .= propDescription
    , "groupName"    .= propGroupName
    , "type"         .= eitherToJSON propType
    , "fieldType"    .= eitherToJSON propFieldType
    , "formField"    .= propFormField
    , "displayOrder" .= propDisplayOrder
    , "options"      .= propOptions
    ]

instance FromJSON Property where
  parseJSON = withObject "Property" $ \o -> do
    Property <$> o .:  "name"
             <*> o .:  "label"
             <*> o .:  "description"
             <*> o .:  "groupName"
             <*> o .:^ "type"
             <*> o .:^ "fieldType"
             <*> o .:  "formField"
             <*> o .:  "displayOrder"
             <*> o .:  "options"

data PropType
  = PTString
  | PTNumber
  | PTBool
  | PTDateTime
  | PTEnumeration
  deriving (Eq, Enum, Bounded, Read, Show)

data PropFieldType
  = PFTTextArea
  | PFTSelect
  | PFTText
  | PFTDate
  | PFTFile
  | PFTNumber
  | PFTRadio
  | PFTCheckBox
  deriving (Eq, Enum, Bounded, Read, Show)

data PropOption = PropOption
  { poLabel        :: !Text
  , poValue        :: !Text
  , poDisplayOrder :: !Int
  }
  deriving Show

--------------------------------------------------------------------------------

-- | Current and previous values of a 'Property'.
data PropValue = PropValue
  { pvalValue    :: !Text
  , pvalVersions :: ![PropVersion]
  }
  deriving Show

instance ToJSON PropValue where
  toJSON PropValue {..} = object $
    [ "value" .= pvalValue
    ] ++
    (pairIf (not . null) "versions" pvalVersions) -- Only included if not empty

instance FromJSON PropValue where
  parseJSON = withObject "PropValue" $ \o -> do
    PropValue <$> o .:  "value"
              <*> o .:* "versions"  -- Empty if not found

-- | A property value with version metadata
data PropVersion = PropVersion
  { pverValue       :: !Text
  , pverSourceType  :: !Text
  , pverSourceId    :: !(Maybe Text)
  , pverSourceLabel :: !(Maybe Text)
  , pverTimestamp   :: !Timestamp
  , pverSelected    :: !Bool
  }
  deriving Show

-- | A map from property names to values
type PropMap = HashMap Text PropValue

--------------------------------------------------------------------------------

-- | A pair for assigning to a property a value.
data SetProp = SetProp
  { spProperty :: !Text
  , spValue    :: !Text
  }
  deriving Show

-- | An intermediate type used only for its 'ToJSON' instance.
data SetPropList = SetPropList { splProperties :: ![SetProp] }

--------------------------------------------------------------------------------

-- | A property group.
--
-- In some cases, the group object returned from HubSpot does not have a
-- @properties@ field. Instead of using a separate type for those cases, we
-- simply return a 'PropGroup' value with an empty 'groupProperties' list.
--
-- https://developers.hubspot.com/docs/methods/contacts/create_group
data PropGroup = PropGroup
  { groupName         :: !Text
  , groupDisplayName  :: !Text
  , groupDisplayOrder :: !Int
  , groupPortalId     :: !PortalId
  , groupProperties   :: ![Property]
  }
  deriving Show

instance ToJSON PropGroup where
  toJSON PropGroup {..} = object $
    [ "name"         .= groupName
    , "displayName"  .= groupDisplayName
    , "displayOrder" .= groupDisplayOrder
    , "portalId"     .= groupPortalId
    ] ++
    (pairIf (not . null) "properties" groupProperties) -- Only included if not empty

instance FromJSON PropGroup where
  parseJSON = withObject "PropGroup" $ \o -> do
    PropGroup <$> o .:  "name"
              <*> o .:  "displayName"
              <*> o .:  "displayOrder"
              <*> o .:  "portalId"
              <*> o .:* "properties"  -- Empty if not found

--------------------------------------------------------------------------------
-- Template Haskell declarations go at the end.

deriveJSON_     ''PropType       (defaultEnumOptions   2)
deriveJSON_     ''PropFieldType  (defaultEnumOptions   3)

deriveJSON_     ''PropOption     (defaultRecordOptions 2)

deriveJSON_     ''PropVersion    (dashedRecordOptions  4)

deriveJSON_     ''SetProp        (defaultRecordOptions 2)
deriveToJSON_   ''SetPropList    (defaultRecordOptions 3)

deriveFromJSON_ ''ContactPage    (dashedRecordOptions  0)
