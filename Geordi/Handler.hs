{-# LANGUAGE GADTs, KindSignatures, TypeOperators, DataKinds, GeneralizedNewtypeDeriving, NamedFieldPuns, RecordWildCards, OverloadedStrings #-}
module Geordi.Handler ( -- * The Handler Type
                        Handler (..)
                      , HandlerStatus (..)
                      , MethodSingleton (..)
                        -- * Handler Monad
                      , HandlerM (..)
                      , runHandlerM
                        -- * Control flow handler actions
                      , done
                      , continue
                        -- * Response primitives
                      , set
                      , respond
                        -- * Calling and Linking Handlers
                      , call
                      , link
                        -- * Low-level WAI interface
                      , ProcessedRequest (..)
                      , processRequest
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
import qualified Data.Map  as M
import qualified Data.Text.Encoding as T

import qualified Geordi.FileInfo as GF 
import Geordi.UrlPattern
import Geordi.FileBackend

newtype HandlerM f x = HandlerM (ReaderT (ProcessedRequest f) (WriterT (Endo Response) (ResourceT IO)) x)
                   deriving ( Monad
                            , Functor
                            , Applicative
                            , MonadIO
                            , MonadWriter (Endo Response)
                            , MonadReader (ProcessedRequest f)
                            , MonadResource
                            , MonadUnsafeIO
                            , MonadActive 
                            , MonadThrow
                            )


runHandlerM :: HandlerM f r -> ProcessedRequest f -> ResourceT IO (r , Response -> Response) 
runHandlerM (HandlerM x) =  fmap (second appEndo) . runWriterT . runReaderT x

data HandlerStatus = Continue
                   | Done

data MethodSingleton :: Method -> * where
  MethodGet  :: MethodSingleton GET
  MethodPost :: MethodSingleton POST
  MethodAny  :: MethodSingleton AnyMethod



data Handler :: * -> [SegmentType *] -> * where
  Handler :: { method  :: MethodSingleton m
             , urlPat  :: UrlPattern m f ts 
             , action  :: (Types ts :--> HandlerM f HandlerStatus) 
             } -> Handler f ts

link :: Handler f ts -> Types (LinkSegments ts) :--> T.Text
link (Handler {urlPat}) = linkUrl urlPat

call :: Handler f ts -> Types ts :--> HandlerM f HandlerStatus
call = action

done :: HandlerM f HandlerStatus
done = return Done

continue :: HandlerM f HandlerStatus
continue = return Continue

set :: (Response -> Response) -> HandlerM f ()
set = tell . Endo

respond :: (Response -> Response) -> HandlerM f HandlerStatus
respond e = set e >> done

data ProcessedRequest f = ProcessedRequest { queries   :: M.Map T.Text [T.Text]
                                           , cookies   :: M.Map T.Text [T.Text] 
                                           , posts     :: M.Map T.Text [T.Text]
                                           , files     :: M.Map T.Text [GF.FileInfo f] 
                                           , urlpieces :: [T.Text]
                                           , methodStr :: H.Method
                                           }

processRequest :: Request -> FileBackend f -> ResourceT IO (ProcessedRequest f)
processRequest req (FB backend') = do
   (posts, files) <- if requestMethod req == H.methodPost
                      then do (params, files) <- parseRequestBody backend' req
                              return ( M.fromListWith (++) . map (T.decodeUtf8 *** ((:[]) . T.decodeUtf8))  $ params
                                     , M.fromListWith (++) . map (T.decodeUtf8 *** ((:[]) . toOurFileInfo)) $ files
                                     )
                      else return (M.empty, M.empty)
   return $ ProcessedRequest { queries = M.fromListWith (++) $ map (second $ (:[]) . fromMaybe "") $ H.queryToQueryText $ queryString req
                             , cookies = M.fromListWith (++) $ map (second (:[])) $ fromMaybe [] $ fmap parseCookiesText $ lookup H.hCookie $ requestHeaders req
                             , urlpieces = pathInfo req 
                             , posts = posts
                             , files = files
                             , methodStr = requestMethod req
                             }
  where toOurFileInfo (FileInfo {..}) = GF.FileInfo fileName fileContentType fileContent

matchWai :: Handler f ts -> ProcessedRequest f -> Maybe (HandlerM f HandlerStatus)
matchWai (Handler {..}) (ProcessedRequest {..}) = case method of
      MethodAny                       -> matchUrl queries posts cookies files urlpieces urlPat action
      x | methodString x == methodStr -> matchUrl queries posts cookies files urlpieces urlPat action
      _                               -> Nothing
  where methodString MethodGet  = H.methodGet
        methodString MethodPost = H.methodPost
        methodString _          = error "Shouldn't be called on MethodAny"

