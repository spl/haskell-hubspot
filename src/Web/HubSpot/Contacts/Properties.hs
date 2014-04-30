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

-- | Update a new contact property (field)
--
-- https://developers.hubspot.com/docs/methods/contacts/update_property
updateProperty
  :: MonadIO m
  => Property
  -> Auth
  -> Manager
  -> m Property
updateProperty prop = generalRequest
  ["https://api.hubapi.com/contacts/v1/properties", propName prop]
  (setMethod POST >=> setJSONBody prop)
  (jsonContent "updateProperty")

-- | Get a property group with all the properties in that group
--
-- https://developers.hubspot.com/docs/methods/contacts/get_groups
getPropGroup
  :: MonadIO m
  => Text  -- ^ Group name
  -> Auth
  -> Manager
  -> m PropGroup
getPropGroup name = generalRequest
  ["https://api.hubapi.com/contacts/v1/groups", name]
  return
  (jsonContent "getPropGroup")

-- | Get all property groups
--
-- https://developers.hubspot.com/docs/methods/contacts/get_groups
getAllPropGroups
  :: MonadIO m
  => Auth
  -> Manager
  -> m [PropGroup]
getAllPropGroups = generalRequest
  ["https://api.hubapi.com/contacts/v1/groups"]
  return
  (jsonContent "getAllPropGroups")

-- | Create a property group
--
-- Note: This doesn't seem to create the properties if you include them in
-- 'groupProperties', so it won't matter whether the list is empty or not
-- (except in terms of the request body size).
--
-- https://developers.hubspot.com/docs/methods/contacts/create_group
createPropGroup
  :: MonadIO m
  => PropGroup
  -> Auth
  -> Manager
  -> m PropGroup
createPropGroup group = generalRequest
  ["https://api.hubapi.com/contacts/v1/groups", groupName group]
  (setMethod PUT >=> setJSONBody group)
  (jsonContent "createPropGroup")

-- | Update a property group
--
-- https://developers.hubspot.com/docs/methods/contacts/update_group
updatePropGroup
  :: MonadIO m
  => PropGroup
  -> Auth
  -> Manager
  -> m PropGroup
updatePropGroup group = generalRequest
  ["https://api.hubapi.com/contacts/v1/groups", groupName group]
  (setMethod POST >=> setJSONBody group)
  (jsonContent "updatePropGroup")
