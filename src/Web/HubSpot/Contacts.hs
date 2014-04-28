module Web.HubSpot.Contacts
  ( getAllContacts
  , getContact
  , getContacts
  , updateContact
  , ContactKey
  ) where

--------------------------------------------------------------------------------

import Web.HubSpot.Common
import Web.HubSpot.Internal
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
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
  => Int  -- ^ Offset into list of all contacts, starts at 0
  -> Int  -- ^ Count (maximum: 100)
  -> Auth
  -> Manager
  -> m ([Contact], Bool, Int)
getAllContacts offset count auth mgr = do
  when (count < 0 || count > 100) $ fail $ "getAllContacts: bad count: " ++ show count
  when (offset < 0) $ fail $ "getAllContacts: bad offset: " ++ show offset
  generalRequest
    ["https://api.hubapi.com/contacts/v1/lists/all/contacts/all"]
    (addQuery [ ("count"     , Just $ intToBS count  )
              , ("vidOffset" , Just $ intToBS offset )
              ])
    (liftM tuplePage . jsonContent "getAllContacts")
    auth
    mgr

-- | Get a contact profile by a key
getContact
  :: (MonadIO m, ContactKey key)
  => key   -- ^ A 'ContactId', a 'UserToken', or a 'Text' email address
  -> Auth
  -> Manager
  -> m Contact
getContact key = generalRequest
  ["https://api.hubapi.com/contacts/v1/contact", keyName key, keyVal key, "profile"]
  return
  (jsonContent "getContact")

-- | Get multiple contact profiles with keys
--
-- A key is either a 'ContactId', a 'UserToken', or a 'Text' email address.
getContacts
  :: (MonadIO m, ContactKey key)
  => [key]   -- ^ List of 'ContactId's, 'UserToken's, or 'Text' email addresses
  -> Auth
  -> Manager
  -> m (HashMap ContactId Contact)
getContacts []   = \_ _ -> return HM.empty
getContacts keys = let key = keyName (head keys) in generalRequest
  ["https://api.hubapi.com/contacts/v1/contact", key `TS.snoc` 's', "batch"]
  (addQuery $ queryTextToQuery $ map ((key,) . Just . keyVal) keys)
  (liftM (HM.fromList . map (first read) . HM.toList) . jsonContent "getContacts")

-- | Update a contact profile by a 'ContactId'
updateContact
  :: MonadIO m
  => ContactId
  -> [PropertyValue]
  -> Auth
  -> Manager
  -> m ()
updateContact contactId propValues = generalRequest
  ["https://api.hubapi.com/contacts/v1/contact/vid", keyVal contactId, "profile"]
  (setJSONBody $ PropValueList propValues)
  (\_ -> return ())

--------------------------------------------------------------------------------

class ContactKey key where
  keyName :: key -> Text
  keyVal  :: key -> Text

-- | https://developers.hubspot.com/docs/methods/contacts/get_contact
instance ContactKey ContactId where
  keyName _ = "vid"
  keyVal = TS.pack . show

-- | https://developers.hubspot.com/docs/methods/contacts/get_contact_by_utk
instance ContactKey UserToken where
  keyName _ = "utk"
  keyVal = TS.decodeUtf8 . fromUserToken

-- | https://developers.hubspot.com/docs/methods/contacts/get_contact_by_email
instance ContactKey Text where
  keyName _ = "email"
  keyVal = id
