{-# LANGUAGE GADTs, KindSignatures, TypeOperators, DataKinds, GeneralizedNewtypeDeriving, NamedFieldPuns, RecordWildCards, OverloadedStrings #-}
module Geordi.Handler ( -- * The Handler Type
                        Handler (..)
                      , HandlerStatus (..)
                      , MethodSingleton (..)
                        -- * Handler Monad
                      , HandlerM (..)
                      , runHandlerM
                        -- * Calling and Linking
                      , call
                      , link
                        -- * Low-level WAI interface
                      , matchWai
                      ) where

import Control.Arrow
import Control.Applicative
import Web.Cookie
import Network.Wai
import Network.Wai.Parse
import Data.Conduit
import Control.Monad.Writer
import Control.Monad.Reader
import Data.Maybe
import Control.Monad.Trans.Resource
import qualified Network.HTTP.Types.URI    as H
import qualified Network.HTTP.Types.Header as H
import qualified Network.HTTP.Types.Method as H
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Geordi.FileInfo as GF 
import Geordi.UrlPattern
import Geordi.FileBackend

newtype HandlerM x = HandlerM (ReaderT Request (WriterT (Endo Response) (ResourceT IO)) x)
                   deriving ( Monad
                            , Functor
                            , Applicative
                            , MonadIO
                            , MonadWriter (Endo Response)
                            , MonadReader Request
                            , MonadResource
                            , MonadUnsafeIO
                            , MonadActive 
                            , MonadThrow
                            )


runHandlerM :: HandlerM r -> Request -> ResourceT IO (r , Response -> Response) 
runHandlerM (HandlerM x) =  fmap (second appEndo) . runWriterT . runReaderT x

data HandlerStatus = Continue
                   | Done

data MethodSingleton :: Method -> * where
  MethodGet  :: MethodSingleton GET
  MethodPost :: MethodSingleton POST

data Handler :: [SegmentType *] -> * where
  Handler :: { method  :: MethodSingleton m
             , backend :: FileBackend f 
             , urlPat  :: UrlPattern m f ts 
             , action  :: (Types ts :--> HandlerM HandlerStatus) 
             } -> Handler ts

link :: Handler ts -> Types (LinkSegments ts) :--> T.Text
link (Handler {urlPat}) = linkUrl urlPat

call :: Handler ts -> Types ts :--> HandlerM HandlerStatus
call = action

matchWai :: Handler ts -> Request -> ResourceT IO (Maybe (HandlerM HandlerStatus))
matchWai (Handler {..}) req 
  | methodString method == requestMethod req 
     = let queries = flip lookup $ map (second $ fromMaybe "") $ H.queryToQueryText $ queryString req
           cookies = maybe (const Nothing) (flip lookup . parseCookiesText) $ lookup H.hCookie $ requestHeaders $ req
           (FB backend') = backend 
        in do (posts, files) <- if requestMethod req == H.methodPost
                                 then do (params, files) <- parseRequestBody backend' req
                                         return ( flip lookup . map (T.decodeUtf8 *** T.decodeUtf8)  $ params
                                                , flip lookup . map (T.decodeUtf8 *** toOurFileInfo) $ files
                                                )
                                 else return (const Nothing, const Nothing)
              return $ matchUrl queries posts cookies files (pathInfo req) urlPat action
  | otherwise = return Nothing
  where methodString MethodGet  = H.methodGet
        methodString MethodPost = H.methodPost
        toOurFileInfo (FileInfo {..}) = GF.FileInfo fileName fileContentType fileContent

