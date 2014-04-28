module Web.HubSpot.Contacts.Properties where

--------------------------------------------------------------------------------

import Web.HubSpot.Common
import Web.HubSpot.Internal

--------------------------------------------------------------------------------

-- | Get all contact properties (fields)
--
-- https://developers.hubspot.com/docs/methods/contacts/get_properties
getAllProperties
  :: MonadIO m
  => Auth
  -> Manager
  -> m [Property]
getAllProperties auth mgr =
  newAuthReq auth ["https://api.hubapi.com/contacts/v1/properties"]
  >>= acceptJSON
  >>= flip httpLbs mgr
  >>= jsonContent "getAllProperties"

-- | Create a new contact property (field)
--
-- https://developers.hubspot.com/docs/methods/contacts/create_property
createProperty
  :: MonadIO m
  => Property
  -> Auth
  -> Manager
  -> m (Either ErrorMessage Property)
createProperty prop auth mgr =
  newAuthReq auth [ "https://api.hubapi.com/contacts/v1/properties"
                  , propName prop
                  ]
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
getGroups
  :: MonadIO m
  => Text  -- ^ Group name
  -> Auth
  -> Manager
  -> m Group
getGroups name auth mgr =
  newAuthReq auth [ "https://api.hubapi.com/contacts/v1/groups"
                  , name
                  ]
  >>= acceptJSON
  >>= flip httpLbs mgr
  >>= jsonContent "getGroups"

-- | Get all property groups
--
-- https://developers.hubspot.com/docs/methods/contacts/get_groups
getAllGroups
  :: MonadIO m
  => Auth
  -> Manager
  -> m [Group]
getAllGroups auth mgr =
  newAuthReq auth ["https://api.hubapi.com/contacts/v1/groups"]
  >>= acceptJSON
  >>= flip httpLbs mgr
  >>= jsonContent "getAllGroups"

-- | Create a property group
--
-- Note: This doesn't seem to create the properties if you include them in
-- 'groupProperties', so it won't matter whether the list is empty or not
-- (except in terms of the request body size).
--
-- https://developers.hubspot.com/docs/methods/contacts/create_group
createGroup
  :: MonadIO m
  => Group
  -> Auth
  -> Manager
  -> m Group
createGroup group auth mgr =
  newAuthReq auth [ "https://api.hubapi.com/contacts/v1/groups"
                  , groupName group
                  ]
  >>= setMethod PUT
  >>= acceptJSON
  >>= setJSONBody group
  >>= flip httpLbs mgr
  >>= jsonContent "createGroup"
