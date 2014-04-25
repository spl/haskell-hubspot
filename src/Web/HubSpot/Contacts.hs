module Web.HubSpot.Contacts
  ( getContacts
  , getContact
  ) where

--------------------------------------------------------------------------------

import Web.HubSpot.Common
import Web.HubSpot.Internal

--------------------------------------------------------------------------------

-- | Get all contacts (in a paginated sort of way)
--
-- Given a desired (maximum) count of contacts and an offset into the entire
-- known list of contacts, this function returns a tuple with the contact list,
-- a boolean which is True if there are more contacts, and the next offset to
-- use to get the next page of contacts.
--
-- https://developers.hubspot.com/docs/methods/contacts/get_contacts
getContacts
  :: MonadIO m
  => Auth
  -> Int  -- ^ Offset into list of all contacts, starts at 0
  -> Int  -- ^ Count (maximum: 100)
  -> Manager
  -> m ([Contact], Bool, Int)
getContacts auth offset count mgr = do
  when (count < 0 || count > 100) $ fail $ "getContacts: bad count: " ++ show count
  when (offset < 0) $ fail $ "getContacts: bad offset: " ++ show offset
  newAuthReq auth "https://api.hubapi.com/contacts/v1/lists/all/contacts/all"
  >>= setQuery [ ("count"     , Just $ intToBS count  )
               , ("vidOffset" , Just $ intToBS offset )
               ]
  >>= acceptJSON
  >>= flip httpLbs mgr
  >>= liftM tuplePage . jsonContent "getContacts"

-- | Get a contact profile by its unique contact ID
--
-- https://developers.hubspot.com/docs/methods/contacts/get_contact
getContact :: MonadIO m => Auth -> ContactId -> Manager -> m Contact
getContact auth contactId mgr =
  newAuthReq auth (  "https://api.hubapi.com/contacts/v1/contact/vid/"
                  <> show contactId
                  <> "/profile")
  >>= acceptJSON
  >>= flip httpLbs mgr
  >>= jsonContent "getContact"
