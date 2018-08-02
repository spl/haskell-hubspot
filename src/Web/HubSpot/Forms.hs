{-# LANGUAGE OverloadedStrings #-}

module Web.HubSpot.Forms
  ( getAllForms
  , getForm
  ) where

--------------------------------------------------------------------------------

import Web.HubSpot.Common
import Web.HubSpot.Internal

--------------------------------------------------------------------------------

-- | Get all forms from a portal
--
-- http://developers.hubspot.com/docs/methods/forms/get_forms
getAllForms
  :: MonadIO m
  => Auth
  -> Manager
  -> m [Form]
getAllForms auth mgr = apiRequest
  ["contacts/v1/forms"]
  return
  (fromJSONResponse "getAllForms")
  auth
  mgr

-- | Get a form by its unique ID
--
-- http://developers.hubspot.com/docs/methods/forms/get_form
getForm
  :: MonadIO m
  => Text
  -> Auth
  -> Manager
  -> m Form
getForm guid = apiRequest
  ["contacts/v1/forms", guid]
  return
  (fromJSONResponse "getForm")
