module Web.HubSpot.Contacts.Properties where

--------------------------------------------------------------------------------

import Web.HubSpot.Common
import Web.HubSpot.Internal

--------------------------------------------------------------------------------

-- | Get all contact properties
--
-- https://developers.hubspot.com/docs/methods/contacts/get_properties
getProperties :: MonadIO m => Auth -> Manager -> m [ContactProperty]
getProperties auth mgr =
  newAuthReq auth "https://api.hubapi.com/contacts/v1/properties"
  >>= acceptJSON
  >>= flip httpLbs mgr
  >>= jsonContent "getProperties"
