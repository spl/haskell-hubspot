module Web.HubSpot.Common
  ( module Web.HubSpot.Common
  , module Prelude
  , module Control.Applicative
  , module Control.Arrow
  , module Control.Monad
  , module Control.Monad.IO.Class
  , module Data.Aeson
  , module Data.Aeson.Types
  , module Data.ByteString
  , module Data.Char
  , module Data.Either
  , module Data.Foldable
  , module Data.Maybe
  , module Data.Monoid
  , module Data.String
  , module Data.Text
  , module Data.Time.Clock
  , module Data.Traversable.Compat
  , module Network.HTTP.Conduit
  , module Network.HTTP.Types
  ) where

--------------------------------------------------------------------------------

import Prelude hiding (mapM, sequence)
import Control.Applicative
import Control.Arrow
import Control.Monad hiding (forM, mapM, sequence)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types
import Data.Char
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Either
import Data.Foldable (foldMap)
import Data.Maybe
import Data.Monoid
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TS
import Data.Time.Clock
import Data.Traversable.Compat
import Network.HTTP.Conduit hiding (parseUrl)
import qualified Network.HTTP.Conduit
import Network.HTTP.Types
import Network.Mime (MimeType)

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

parseUrl :: MonadIO m => String -> m Request
parseUrl = liftIO . Network.HTTP.Conduit.parseUrl

--------------------------------------------------------------------------------

setMethod :: Monad m => StdMethod -> Request -> m Request
setMethod m req = return $ req { method = renderStdMethod m }

--------------------------------------------------------------------------------

setQuery :: Monad m => Query -> Request -> m Request
setQuery q req = return $ req { queryString = renderQuery True q }

addQueryItem :: Monad m => QueryItem -> Request -> m Request
addQueryItem qi req = return $
  req { queryString = renderQuery True $ qi : parseQuery (queryString req) }

lookupQ :: ByteString -> Query -> Maybe ByteString
lookupQ k = join . lookup k

--------------------------------------------------------------------------------

addHeader :: Monad m => Header -> Request -> m Request
addHeader hdr req = return $ req { requestHeaders = hdr : requestHeaders req }

findHeader :: Monad m => HeaderName -> Response b -> m ByteString
findHeader hdr rsp = case lookup hdr $ responseHeaders rsp of
  Nothing -> fail $  "findHeader: Can't find " ++ show hdr
                  ++ " in: " ++ show (responseHeaders rsp)
  Just val -> return val

--------------------------------------------------------------------------------

mimeTypeContent :: Monad m => Response BL.ByteString -> m (MimeType, BL.ByteString)
mimeTypeContent rsp =
  (,) `liftM` findHeader hContentType rsp
      `ap`    return (responseBody rsp)

--------------------------------------------------------------------------------

setUrlEncodedBody :: Monad m => [(ByteString, ByteString)] -> Request -> m Request
setUrlEncodedBody body req = return $ urlEncodedBody body req

--------------------------------------------------------------------------------

contentTypeJSON :: ByteString
contentTypeJSON = "application/json"

acceptJSON :: Monad m => Request -> m Request
acceptJSON = addHeader (hAccept, contentTypeJSON)

jsonContent :: (Monad m, FromJSON a) => String -> Response BL.ByteString -> m a
jsonContent msg rsp = do
  (contentType, body) <- mimeTypeContent rsp
  if contentType == contentTypeJSON then
    maybe (fail $ msg ++ ": Can't decode JSON from response: " ++ show rsp)
          return
          (decode' body)
  else
    fail $ msg ++ ": unknown content type: " ++ show contentType
