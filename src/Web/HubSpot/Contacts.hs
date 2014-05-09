module Web.HubSpot.Contacts
  ( getAllContacts
  , getContact
  , getContacts
  , updateContact
  , createOrUpdateContact
  , createOrUpdateContacts
  , ContactKey
  ) where

--------------------------------------------------------------------------------

import Web.HubSpot.Common
import Web.HubSpot.Internal
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
    (liftM fromContactPage . jsonContent "getAllContacts")
    auth
    mgr

-- | Get a contact profile by a key
--
-- By 'ContactId': https://developers.hubspot.com/docs/methods/contacts/get_contact
--
-- By 'UserToken': https://developers.hubspot.com/docs/methods/contacts/get_contact_by_utk
--
-- By email address ('Text'): https://developers.hubspot.com/docs/methods/contacts/get_contact_by_email
getContact
  :: (MonadIO m, ContactKey key)
  => key   -- ^ 'ContactId', 'UserToken', or email address ('Text')
  -> Auth
  -> Manager
  -> m Contact
getContact key = generalRequest
  ["https://api.hubapi.com/contacts/v1/contact", keyName key, keyVal key, "profile"]
  return
  (jsonContent "getContact")

-- | Get multiple contact profiles by keys
--
-- By 'ContactId': https://developers.hubspot.com/docs/methods/contacts/get_batch_by_vid
--
-- By 'UserToken': https://developers.hubspot.com/docs/methods/contacts/get_contact_by_utk
--
-- By email address ('Text'): https://developers.hubspot.com/docs/methods/contacts/get_batch_by_email
getContacts
  :: (MonadIO m, ContactKey key)
  => [key]   -- ^ @key@ is 'ContactId', 'UserToken', or email address ('Text')
  -> Auth
  -> Manager
  -> m [(ContactId, Contact)]
getContacts []   = \_ _ -> return []
getContacts keys = let key = keyName (head keys) in generalRequest
  ["https://api.hubapi.com/contacts/v1/contact", key `TS.snoc` 's', "batch"]
  (addQuery $ queryTextToQuery $ map ((key,) . Just . keyVal) keys)
  (liftM (map (first read) . HM.toList) . jsonContent "getContacts")

-- | Update a contact profile by a 'ContactId'
--
-- https://developers.hubspot.com/docs/methods/contacts/update_contact
updateContact
  :: MonadIO m
  => ContactId
  -> [SetProp]
  -> Auth
  -> Manager
  -> m ()
updateContact contactId setProps = generalRequest
  ["https://api.hubapi.com/contacts/v1/contact/vid", keyVal contactId, "profile"]
  (setJSONBody $ SetPropList setProps)
  (\_ -> return ())

-- | Create or update a contact profile by email addresses
--
-- https://developers.hubspot.com/docs/methods/contacts/create_or_update
createOrUpdateContact
  :: MonadIO m
  => Email
  -> [SetProp]
  -> Auth
  -> Manager
  -> m (ContactId, Bool)
createOrUpdateContact email setProps = generalRequest
  ["https://api.hubapi.com/contacts/v1/contact/createOrUpdate/email/", urlEncodeText False email]
  (setJSONBody $ SetPropList setProps)
  (jsonContent "createOrUpdateContact")

-- | Create or update multiple contact profiles by email addresses
--
-- https://developers.hubspot.com/docs/methods/contacts/batch_create_or_update
createOrUpdateContacts
  :: MonadIO m
  => [(Email, [SetProp])]  -- ^ Each pair consists of a 'Text' email address and properties
  -> Auth
  -> Manager
  -> m ()
createOrUpdateContacts emailAndProps = generalRequest
  ["https://api.hubapi.com/contacts/v1/contact/batch"]
  (setJSONBody $ map toUpdateContact emailAndProps)
  (\_ -> return ())

--------------------------------------------------------------------------------

class ContactKey key where
  keyName :: key -> Text
  keyVal  :: key -> Text

instance ContactKey ContactId where
  keyName _ = "vid"
  keyVal = TS.pack . show

instance ContactKey UserToken where
  keyName _ = "utk"
  keyVal = TS.decodeUtf8 . fromUserToken

instance ContactKey Text where
  keyName _ = "email"
  keyVal = id
