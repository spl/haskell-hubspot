module Web.HubSpot.Contacts.Properties where

--------------------------------------------------------------------------------

import Web.HubSpot.Common
import Web.HubSpot.Internal

--------------------------------------------------------------------------------

-- | Get a contact property (fields)
--
-- https://developers.hubspot.com/docs/methods/contacts/get_properties
getProperty
  :: MonadIO m
  => Text  -- ^ Property name
  -> Auth
  -> ManageRequest
  -> m Property
getProperty name = apiRequest
  ["contacts/v1/properties", name]
  return
  (fromJSONResponse "getProperty")

-- | Get all contact properties (fields)
--
-- https://developers.hubspot.com/docs/methods/contacts/get_properties
getAllProperties
  :: MonadIO m
  => Auth
  -> ManageRequest
  -> m [Property]
getAllProperties = apiRequest
  ["contacts/v1/properties"]
  return
  (fromJSONResponse "getAllProperties")

-- | Create a new contact property (field)
--
-- https://developers.hubspot.com/docs/methods/contacts/create_property
createProperty
  :: MonadIO m
  => Property
  -> Auth
  -> ManageRequest
  -> m Property
createProperty prop = apiRequest
  ["contacts/v1/properties", propName prop]
  (setJSONBody PUT prop)
  (fromJSONResponse "createProperty")

-- | Update a new contact property (field)
--
-- https://developers.hubspot.com/docs/methods/contacts/update_property
updateProperty
  :: MonadIO m
  => Property
  -> Auth
  -> ManageRequest
  -> m Property
updateProperty prop = apiRequest
  ["contacts/v1/properties", propName prop]
  (setJSONBody POST prop)
  (fromJSONResponse "updateProperty")

-- | Get a property group with all the properties in that group
--
-- https://developers.hubspot.com/docs/methods/contacts/get_groups
getPropGroup
  :: MonadIO m
  => Text  -- ^ Group name
  -> Auth
  -> ManageRequest
  -> m PropGroup
getPropGroup name = apiRequest
  ["contacts/v1/groups", name]
  return
  (fromJSONResponse "getPropGroup")

-- | Get all property groups
--
-- https://developers.hubspot.com/docs/methods/contacts/get_groups
getAllPropGroups
  :: MonadIO m
  => Auth
  -> ManageRequest
  -> m [PropGroup]
getAllPropGroups = apiRequest
  ["contacts/v1/groups"]
  return
  (fromJSONResponse "getAllPropGroups")

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
  -> ManageRequest
  -> m PropGroup
createPropGroup group = apiRequest
  ["contacts/v1/groups", groupName group]
  (setJSONBody PUT group)
  (fromJSONResponse "createPropGroup")

-- | Update a property group
--
-- https://developers.hubspot.com/docs/methods/contacts/update_group
updatePropGroup
  :: MonadIO m
  => PropGroup
  -> Auth
  -> ManageRequest
  -> m PropGroup
updatePropGroup group = apiRequest
  ["contacts/v1/groups", groupName group]
  (setJSONBody POST group)
  (fromJSONResponse "updatePropGroup")
