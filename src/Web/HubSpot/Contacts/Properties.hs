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
getAllProperties = generalRequest
  ["https://api.hubapi.com/contacts/v1/properties"]
  return
  (jsonContent "getAllProperties")

-- | Create a new contact property (field)
--
-- https://developers.hubspot.com/docs/methods/contacts/create_property
createProperty
  :: MonadIO m
  => Property
  -> Auth
  -> Manager
  -> m Property
createProperty prop = generalRequest
  ["https://api.hubapi.com/contacts/v1/properties", propName prop]
  (setMethod PUT >=> setJSONBody prop)
  (jsonContent "createProperty")

-- | Get property groups for a given group
--
-- https://developers.hubspot.com/docs/methods/contacts/get_groups
getGroups
  :: MonadIO m
  => Text  -- ^ Group name
  -> Auth
  -> Manager
  -> m Group
getGroups name = generalRequest
  ["https://api.hubapi.com/contacts/v1/groups", name]
  return
  (jsonContent "getGroups")

-- | Get all property groups
--
-- https://developers.hubspot.com/docs/methods/contacts/get_groups
getAllGroups
  :: MonadIO m
  => Auth
  -> Manager
  -> m [Group]
getAllGroups = generalRequest
  ["https://api.hubapi.com/contacts/v1/groups"]
  return
  (jsonContent "getAllGroups")

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
createGroup group = generalRequest
  ["https://api.hubapi.com/contacts/v1/groups", groupName group]
  (setMethod PUT >=> setJSONBody group)
  (jsonContent "createGroup")
