{-# LANGUAGE RecordWildCards, GADTs, TupleSections, OverloadedStrings #-}
module Geordi.Response.Wai where

import qualified Data.CaseInsensitive as CI
import Geordi.Response
import qualified Network.Wai as W
import qualified Web.Cookie  as W
import qualified Data.Map as M
import Geordi.Util.Exists
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy.Encoding as T
import Control.Arrow
import qualified Network.HTTP.Types.Header as H
import qualified Blaze.ByteString.Builder as B

toWai :: Response -> W.Response
toWai (Response {..}) = case responseBody of 
   File path (Just (s,e)) -> W.ResponseFile responseStatus headers path (Just $ W.FilePart s e)
   File path Nothing      -> W.ResponseFile responseStatus headers path Nothing
   Builder builder -> W.ResponseBuilder responseStatus headers builder
   Source source   -> W.ResponseSource  responseStatus headers source
  where headers = map (second ick) (M.toList responseHeaders) ++ compileCookies responseCookies
        ick = B.toStrict . T.encodeUtf8
        compileCookies = map (toCookieHeader . toSetCookie)
        toSetCookie :: Exists Cookie -> W.SetCookie
        toSetCookie (ExI c) = toSetCookie' W.def c
          where toSetCookie' :: W.SetCookie -> Cookie f -> W.SetCookie
                toSetCookie' c (a := b) = c { W.setCookieName = ick a, W.setCookieValue = ick b}
                toSetCookie' c (Path t n) = toSetCookie' (c { W.setCookiePath = Just $ ick t }) n
                toSetCookie' c (Domain t n) = toSetCookie' (c { W.setCookieDomain = Just $ ick t }) n 
                toSetCookie' c (Expires t n) = toSetCookie' (c { W.setCookieExpires = Just t }) n 
                toSetCookie' c (MaxAge t n) = toSetCookie' (c { W.setCookieMaxAge = Just t }) n 
                toSetCookie' c (HttpOnly n) = toSetCookie' (c { W.setCookieHttpOnly = True }) n
                toSetCookie' c (Secure n) = toSetCookie' (c { W.setCookieSecure = True }) n
        toCookieHeader = ("Set-Cookie",) . B.toByteString . W.renderSetCookie 
