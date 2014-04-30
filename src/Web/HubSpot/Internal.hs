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
  -> (Request -> m Request)
  -> (Response BL.ByteString -> m a)
  -> Auth
  -> Manager
  -> m a
generalRequest urlSegments modifyRequest processResponse Auth {..} mgr =
  parseUrl (TS.unpack $ TS.intercalate "/" urlSegments)
  >>= acceptJSON
  >>= setQuery [("access_token", Just authAccessToken)]
  >>= modifyRequest
  >>= flip httpLbs mgr
  >>= checkResponse
  >>= processResponse

-- | Check the response HTTP status codes. Throw an exception if needed.
checkResponse :: MonadIO m => Response BL.ByteString -> m (Response BL.ByteString)
checkResponse rsp = do
  let status = responseStatus rsp
      body = responseBody rsp
  case statusCode status of
    200 -> return rsp
    204 -> return rsp
    401 -> liftIO $ throwIO $ UnauthorizedRequest body
    _   -> liftIO $ throwIO $ UnexpectedHttpResponse status body

--------------------------------------------------------------------------------

-- | Exceptions thrown by functions in this package.
data HubSpotException
    -- | A refresh was attempted, but no refresh token was provided.
  = NoRefreshToken
    -- | HTTP Access Code 401 (Unauthorized) which indicates when an Access
    -- Token has expired. The response body is included.
    --
    -- See https://developers.hubspot.com/auth/oauth_apps
  | UnauthorizedRequest !BL.ByteString
    -- | An unexpected HTTP response status with the response body
  | UnexpectedHttpResponse !Status !BL.ByteString
  deriving (Show, Typeable)

instance Exception HubSpotException

--------------------------------------------------------------------------------

-- | Access token
type AccessToken = ByteString

-- | Refresh token
type RefreshToken = ByteString

-- | Client ID
type ClientId = ByteString

-- | Portal ID (sometimes called Hub ID or account number)
type PortalId = Int

-- | An OAuth scope from https://developers.hubspot.com/auth/oauth_scopes
type Scope = ByteString

--------------------------------------------------------------------------------

-- | Authentication information
data Auth = Auth
  { authClientId     :: !ClientId             -- ^ HubSpot app identifier
  , authPortalId     :: !PortalId             -- ^ HubSpot user identifier
  , authAccessToken  :: !AccessToken          -- ^ Access token for OAuth
  , authRefreshToken :: !(Maybe RefreshToken) -- ^ Only for "offline" scope
  , authExpiration   :: !UTCTime              -- ^ Expiration time of the access token
  }
  deriving Show

instance ToJSON Auth where
  toJSON Auth {..} = object
    [ "client_id"     .= TS.decodeUtf8 authClientId
    , "portal_id"     .= authPortalId
    , "access_token"  .= TS.decodeUtf8 authAccessToken
    , "refresh_token" .= fmap TS.decodeUtf8 authRefreshToken
    , "expiration"    .= authExpiration
    ]

instance FromJSON Auth where
  parseJSON = withObject "Auth" $ \o -> do
    Auth <$> (TS.encodeUtf8 <$> o .: "client_id")
         <*> o .: "portal_id"
         <*> (TS.encodeUtf8 <$> o .: "access_token")
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

-- | A contact profile from HubSpot
--
-- Note: This is a simple type synonym. We should use a real data type.
type Contact = HashMap Text Value

-- | An unexported, intermediate type used in getContacts
data ContactsPage = ContactsPage
  { contacts   :: ![Contact]
  , has_more   :: !Bool
  , vid_offset :: !Int
  }

tuplePage :: ContactsPage -> ([Contact], Bool, Int)
tuplePage (ContactsPage a b c) = (a, b, c)

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

-- | This is used to set the value of a property on a contact.
data PropValue = PropValue
  { pvName  :: !Text
  , pvValue :: !Text
  }

instance ToJSON PropValue where
  toJSON PropValue {..} = object
    [ "property" .= pvName
    , "value"    .= pvValue
    ]

instance FromJSON PropValue where
  parseJSON = withObject "PropValue" $ \o -> do
    PropValue <$> o .: "property"
              <*> o .: "value"

-- | An unexported, intermediate type used for retrieving a list of
-- 'PropValue's.
data PropValueList = PropValueList { pvlProperties  :: ![PropValue] }

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
  , groupProperties   :: ![Property] -- ^ This list is empty if no properties field is available.
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

deriveJSON_ ''PropType          (defaultEnumOptions   2)
deriveJSON_ ''PropFieldType     (defaultEnumOptions   3)

deriveJSON_ ''PropOption        (defaultRecordOptions 2)
deriveJSON_ ''PropValueList     (defaultRecordOptions 3)

deriveJSON_ ''ContactsPage
  defaultOptions { fieldLabelModifier = map (\c -> if c == '_' then '-' else c) }
