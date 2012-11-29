{-# LANGUAGE GADTs, DataKinds, KindSignatures, OverloadedStrings #-}
module Geordi.Response where

import Geordi.Util.Exists

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.ByteString
import Blaze.ByteString.Builder.Char.Utf8
import Data.Conduit
import qualified Network.HTTP.Types.Header as H
import qualified Network.HTTP.Types.Status as H
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E
import Data.Time
data Response = Response { responseStatus  :: H.Status
                         , responseCookies :: [Exists Cookie]
                         , responseHeaders :: M.Map H.HeaderName T.Text
                         , responseBody    :: ResponseBody
                         }
data ResponseBody = File FilePath (Maybe (Integer, Integer))
                  | Builder Builder
                  | Source (Source (ResourceT IO) (Flush Builder))

status :: H.Status -> Response -> Response
status h r = r { responseStatus = h }

data CookieExpiration = None | Date | Offset

data Cookie :: CookieExpiration -> * where
  (:=)  :: T.Text -> T.Text -> Cookie None
  Expires :: UTCTime -> Cookie None -> Cookie Date
  MaxAge  :: DiffTime -> Cookie None -> Cookie Offset
  Domain  :: T.Text -> Cookie a -> Cookie a
  Path    :: T.Text -> Cookie a -> Cookie a
  HttpOnly :: Cookie a -> Cookie a
  Secure   :: Cookie a -> Cookie a

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

cookie :: Cookie e -> Response -> Response
cookie c r = r { responseCookies = ExI c : responseCookies r }

header :: H.HeaderName -> T.Text -> Response -> Response
header k v r = r { responseHeaders = M.insert k v $ responseHeaders r }

file :: FilePath -> Response -> Response
file p r = r { responseBody = File p Nothing }

builder :: Builder -> Response -> Response
builder p r = r { responseBody = Builder p }

source :: Source (ResourceT IO) (Flush Builder) -> Response -> Response
source s r = r { responseBody = Source s }

contentType :: T.Text -> Response -> Response
contentType = header H.hContentType

text :: T.Text -> Response -> Response
text = builder . fromLazyText

bytestring :: B.ByteString -> Response -> Response
bytestring = builder . fromLazyByteString

emptyResponse :: Response 
emptyResponse = Response H.status500 [] M.empty (Builder $ fromLazyText "") 
