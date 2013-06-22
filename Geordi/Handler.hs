{-# LANGUAGE GADTs, ScopedTypeVariables, UndecidableInstances, FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, KindSignatures, PolyKinds, FlexibleContexts, TypeOperators, DataKinds, GeneralizedNewtypeDeriving, NamedFieldPuns, RecordWildCards, OverloadedStrings, RankNTypes #-}
module Geordi.Handler ( -- * The Handler Type
                        Handler (..)
                      , HandlerStatus (..)
                      , MethodSingleton (..)
                        -- * Handler Monad
                      , HandlerM (..)
                      , runHandlerM
                      , MonadHandler (..)
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
                        -- * Shifting monads in handlers
                      , monadSuffixH
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

class MonadHandler m f | m -> f where
  liftHandler :: HandlerM f x -> m x
instance MonadHandler (HandlerM f) f where
  liftHandler = id
instance (Monad m, MonadTrans t, MonadHandler m f) => MonadHandler (t m) f where
  liftHandler = lift . liftHandler

data Handler :: (* -> *) -> * -> [SegmentType *] -> * where
  Handler :: (MonadHandler m f) => { method  :: MethodSingleton method
                                   , urlPat  :: UrlPattern method f ts
                                   , action  :: (Types ts :--> m HandlerStatus)
                                   } -> Handler m f ts


monadSuffixH :: forall m2 m f n ts. (MonadHandler m2 f, MonadHandler m f)
            => (forall method. UrlPattern method f n)
            -> (m HandlerStatus -> Types n :--> m2 HandlerStatus)
            -> Handler m f ts
            -> Handler m2 f (ts :++: n)
monadSuffixH p f (Handler {method, urlPat, action})
   | Refl <- collectProof (undefined :: m2 HandlerStatus) (typesWitness urlPat) (typesWitness p)
   , Refl <- typesProof p urlPat
   = Handler { method = method
             , urlPat = urlPat // p
             , action = mapMany (typesWitness urlPat) f action
             }

link :: Handler m f ts -> Types (LinkSegments ts) :--> T.Text
link (Handler {urlPat}) = linkUrl urlPat

call :: Handler m f ts -> Types ts :--> m HandlerStatus
call = action

done :: Monad m => m HandlerStatus
done = return Done

continue :: Monad m => m HandlerStatus
continue = return Continue

set :: (MonadWriter (Endo f) m) => (f -> f) -> m ()
set = tell . Endo

respond :: (Monad m, MonadHandler m f) => (Response -> Response) -> m HandlerStatus
respond e = liftHandler (set e) >> done

matchRequest :: Handler m f ts -> Request f -> Maybe (m HandlerStatus)
matchRequest (Handler {..}) (Request {..}) = case method of
      MethodAny                       -> matchUrl queries posts cookies files urlpieces urlPat action
      x | methodString x == methodStr -> matchUrl queries posts cookies files urlpieces urlPat action
      _                               -> Nothing
  where methodString MethodGet  = H.methodGet
        methodString MethodPost = H.methodPost
        methodString _          = error "Shouldn't be called on MethodAny"

