module Web.HubSpot.Common
  ( module Web.HubSpot.Common
  , module Prelude
  , module Control.Applicative
  , module Control.Arrow
  , module Control.Monad
  , module Control.Monad.IO.Class
  , module Data.ByteString
  , module Data.Char
  , module Data.Foldable
  , module Data.Monoid
  , module Data.String
  , module Data.Text
  , module Network.HTTP.Conduit
  , module Network.HTTP.Types
  ) where

--------------------------------------------------------------------------------

import Prelude
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (foldMap)
import Data.Monoid
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TS
import Network.HTTP.Conduit
import Network.HTTP.Types

#if MIN_VERSION_bytestring(0,10,2)
import Data.ByteString.Builder (toLazyByteString, intDec)
#else
import Data.ByteString.Lazy.Builder (toLazyByteString)
import Data.ByteString.Lazy.Builder.ASCII (intDec)
#endif

--------------------------------------------------------------------------------

showBS:: ByteString -> String
showBS = TS.unpack . TS.decodeUtf8

intToBS :: Int -> ByteString
intToBS = BL.toStrict . toLazyByteString . intDec

intFromBS :: ByteString -> Maybe Int
intFromBS = readMay . showBS

readMay :: Read a => String -> Maybe a
readMay s = case [x | (x, t) <- reads s, ("", "") <- lex t] of
  [x] -> Just x
  _   -> Nothing
--------------------------------------------------------------------------------

setQuery :: Monad m => Query -> Request -> m Request
setQuery q req = return $ req { queryString = renderQuery True q }

lookupQ :: ByteString -> Query -> Maybe ByteString
lookupQ k = join . lookup k
