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
                      , matchRequest
                      ) where

import Control.Arrow
import Control.Applicative
import Data.Maybe
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import qualified Network.HTTP.Types.Method as H
import qualified Data.Text.Lazy as T
import qualified Data.Map  as M
import qualified Data.Text.Lazy.Encoding as T

import Geordi.UrlPattern
import Geordi.FileBackend
import Geordi.Request
import Geordi.Response

newtype HandlerM f x = HandlerM (ReaderT (Request f) (WriterT (Endo Response) (ResourceT IO)) x)
                   deriving ( Monad
                            , Functor
                            , Applicative
                            , MonadIO
                            , MonadWriter (Endo Response)
                            , MonadReader (Request f)
                            , MonadResource
                            , MonadUnsafeIO
                            , MonadActive 
                            , MonadThrow
                            )


runHandlerM :: HandlerM f r -> Request f -> ResourceT IO (r , Response -> Response) 
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

matchRequest :: Handler f ts -> Request f -> Maybe (HandlerM f HandlerStatus)
matchRequest (Handler {..}) (Request {..}) = case method of
      MethodAny                       -> matchUrl queries posts cookies files urlpieces urlPat action
      x | methodString x == methodStr -> matchUrl queries posts cookies files urlpieces urlPat action
      _                               -> Nothing
  where methodString MethodGet  = H.methodGet
        methodString MethodPost = H.methodPost
        methodString _          = error "Shouldn't be called on MethodAny"

