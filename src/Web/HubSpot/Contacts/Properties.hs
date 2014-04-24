module Web.HubSpot.Contacts.Properties where

--------------------------------------------------------------------------------

import Web.HubSpot.Common
import Web.HubSpot.Internal
import qualified Data.Text as TS

--------------------------------------------------------------------------------

-- | Get all contact properties (fields)
--
-- https://developers.hubspot.com/docs/methods/contacts/get_properties
getProperties :: MonadIO m => Auth -> Manager -> m [ContactProperty]
getProperties auth mgr =
  newAuthReq auth "https://api.hubapi.com/contacts/v1/properties"
  >>= acceptJSON
  >>= flip httpLbs mgr
  >>= jsonContent "getProperties"

-- | Create a new contact property (field)
--
-- https://developers.hubspot.com/docs/methods/contacts/create_property
createProperty
  :: MonadIO m
  => Auth
  -> ContactProperty
  -> Manager
  -> m (Either ErrorMessage ContactProperty)
createProperty auth prop mgr =
  newAuthReq auth (TS.unpack $ "https://api.hubapi.com/contacts/v1/properties/" <> cpName prop)
  >>= setMethod PUT
  >>= acceptJSON
  >>= setJSONBody prop
  >>= flip httpLbs mgr
  >>= \rsp -> case statusCode $ responseStatus rsp of
    200 -> Right `liftM` jsonContent "createProperty" rsp
    _   -> Left `liftM` jsonContent "createProperty" rsp

-- | Get property groups for a given group
--
-- https://developers.hubspot.com/docs/methods/contacts/get_groups
getGroupProperties
  :: MonadIO m
  => Auth
  -> Text  -- ^ Group name
  -> Manager
  -> m GroupProperties
getGroupProperties auth groupName mgr =
  newAuthReq auth (TS.unpack $ "https://api.hubapi.com/contacts/v1/groups/" <> groupName)
  >>= acceptJSON
  >>= flip httpLbs mgr
  >>= jsonContent "getGroupProperties"

-- | Get all group properties
--
-- https://developers.hubspot.com/docs/methods/contacts/get_groups
getAllGroupProperties
  :: MonadIO m
  => Auth
  -> Manager
  -> m [GroupProperties]
getAllGroupProperties auth mgr =
  newAuthReq auth "https://api.hubapi.com/contacts/v1/groups"
  >>= acceptJSON
  >>= flip httpLbs mgr
  >>= jsonContent "getAllGroupProperties"
