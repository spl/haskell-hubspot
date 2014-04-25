module Web.HubSpot.Contacts
  ( getAllContacts
  , getContact
  , ContactKey
  ) where

--------------------------------------------------------------------------------

import Web.HubSpot.Common
import Web.HubSpot.Internal
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TS

--------------------------------------------------------------------------------

-- | Get all contacts (in a paginated sort of way)
--
-- Given a desired (maximum) count of contacts and an offset into the entire
-- known list of contacts, this function returns a tuple with the contact list,
-- a boolean which is True if there are more contacts, and the next offset to
-- use to get the next page of contacts.
--
-- https://developers.hubspot.com/docs/methods/contacts/get_contacts
getAllContacts
  :: MonadIO m
  => Auth
  -> Int  -- ^ Offset into list of all contacts, starts at 0
  -> Int  -- ^ Count (maximum: 100)
  -> Manager
  -> m ([Contact], Bool, Int)
getAllContacts auth offset count mgr = do
  when (count < 0 || count > 100) $ fail $ "getAllContacts: bad count: " ++ show count
  when (offset < 0) $ fail $ "getAllContacts: bad offset: " ++ show offset
  newAuthReq auth ["https://api.hubapi.com/contacts/v1/lists/all/contacts/all"]
  >>= addQuery [ ("count"     , Just $ intToBS count  )
               , ("vidOffset" , Just $ intToBS offset )
               ]
  >>= acceptJSON
  >>= flip httpLbs mgr
  >>= liftM tuplePage . jsonContent "getAllContacts"

-- | Get a contact profile by a key
--
-- The key is either a 'ContactId', a 'UserToken', or a 'Text' email address.
getContact :: (MonadIO m, ContactKey key) => Auth -> key -> Manager -> m Contact
getContact auth key mgr =
  newAuthReq auth [ "https://api.hubapi.com/contacts/v1/contact"
                  , contactRoute key
                  , contactKey key
                  , "profile"
                  ]
  >>= acceptJSON
  >>= flip httpLbs mgr
  >>= jsonContent "getContact"

--------------------------------------------------------------------------------

class ContactKey key where
  contactRoute  :: key -> Text
  contactKey    :: key -> Text

-- | https://developers.hubspot.com/docs/methods/contacts/get_contact
instance ContactKey ContactId where
  contactRoute _ = "vid"
  contactKey = TS.pack . show

-- | https://developers.hubspot.com/docs/methods/contacts/get_contact_by_utk
instance ContactKey UserToken where
  contactRoute _ = "utk"
  contactKey = TS.decodeUtf8 . fromUserToken

-- | https://developers.hubspot.com/docs/methods/contacts/get_contact_by_email
instance ContactKey Text where
  contactRoute _ = "email"
  contactKey = id
