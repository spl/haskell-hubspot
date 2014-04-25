module Web.HubSpot.Contacts
  ( getAllContacts
  , getContact
  , getContacts
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
                  , contactKey key
                  , contactKeyVal key
                  , "profile"
                  ]
  >>= acceptJSON
  >>= flip httpLbs mgr
  >>= jsonContent "getContact"

-- | Get multiple contact profiles with keys
--
-- A key is either a 'ContactId', a 'UserToken', or a 'Text' email address.
getContacts
  :: (MonadIO m, ContactKey key)
  => Auth
  -> [key]
  -> Manager
  -> m (HashMap ContactId Contact)
getContacts _    []   _   = return HM.empty
getContacts auth keys mgr = let key = contactKey (head keys) in
  newAuthReq auth [ "https://api.hubapi.com/contacts/v1/contact"
                  , key `TS.snoc` 's'
                  , "batch"
                  ]
  >>= addQuery (queryTextToQuery $ map ((key,) . Just . contactKeyVal) keys)
  >>= acceptJSON
  >>= flip httpLbs mgr
  >>= liftM (HM.fromList . map (first read) . HM.toList) . jsonContent "getContacts"

--------------------------------------------------------------------------------

class ContactKey key where
  contactKey    :: key -> Text
  contactKeyVal :: key -> Text

-- | https://developers.hubspot.com/docs/methods/contacts/get_contact
instance ContactKey ContactId where
  contactKey _ = "vid"
  contactKeyVal = TS.pack . show

-- | https://developers.hubspot.com/docs/methods/contacts/get_contact_by_utk
instance ContactKey UserToken where
  contactKey _ = "utk"
  contactKeyVal = TS.decodeUtf8 . fromUserToken

-- | https://developers.hubspot.com/docs/methods/contacts/get_contact_by_email
instance ContactKey Text where
  contactKey _ = "email"
  contactKeyVal = id
