{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Geordi.Request.Wai where

import Geordi.Request
import Geordi.FileInfo
import Geordi.FileBackend
import qualified Data.ByteString.Lazy      as B
import qualified Network.Wai               as W
import qualified Network.Wai.Parse         as W
import qualified Network.HTTP.Types.URI    as H
import qualified Network.HTTP.Types.Header as H
import qualified Network.HTTP.Types.Method as H
import qualified Web.Cookie                as W
import qualified Data.Map                  as M
import Control.Monad.Trans.Resource
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy          as T
import Data.Maybe
import Control.Arrow


fromWai :: W.Request -> FileBackend f -> ResourceT IO (Request f)
fromWai req (FB backend') = do
   (posts, files) <- if W.requestMethod req == H.methodPost
                      then do (params, files) <- W.parseRequestBody backend' req
                              return ( M.fromListWith (++) . map (T.decodeUtf8 . B.fromStrict *** ((:[]) . T.decodeUtf8 . B.fromStrict))  $ params
                                     , M.fromListWith (++) . map (T.decodeUtf8 . B.fromStrict *** ((:[]) . toOurFileInfo)) $ files
                                     )
                      else return (M.empty, M.empty)
   return $ Request { queries = M.fromListWith (++) $ map (T.fromStrict *** (:[]) . T.fromStrict . fromMaybe "") $ H.queryToQueryText $ W.queryString req
                    , cookies = M.fromListWith (++) $ map (T.fromStrict *** (:[]) . T.fromStrict) $ fromMaybe [] $ fmap W.parseCookiesText $ lookup H.hCookie $ W.requestHeaders req
                    , urlpieces = map T.fromStrict $ W.pathInfo req 
                    , posts = posts
                    , files = files
                    , methodStr = W.requestMethod req
                    }
  where toOurFileInfo (W.FileInfo {..}) = FileInfo fileName fileContentType fileContent
