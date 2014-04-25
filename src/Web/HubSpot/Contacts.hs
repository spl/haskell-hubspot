module Web.HubSpot.Contacts where

--------------------------------------------------------------------------------

import Web.HubSpot.Common
import Web.HubSpot.Internal
import Data.HashMap.Strict (HashMap)

--------------------------------------------------------------------------------

-- | Get a contact profile by its unique contact ID
--
-- https://developers.hubspot.com/docs/methods/contacts/get_contact
getContact :: MonadIO m => Auth -> ContactId -> Manager -> m (HashMap Text Value)
getContact auth contactId mgr =
  newAuthReq auth (  "https://api.hubapi.com/contacts/v1/contact/vid/"
                  <> show contactId
                  <> "/profile")
  >>= acceptJSON
  >>= flip httpLbs mgr
  >>= jsonContent "getContact"
