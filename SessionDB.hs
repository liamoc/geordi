{-# LANGUAGE GADTs, TypeOperators, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, DataKinds, PolyKinds, RankNTypes, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
module SessionDB  where

import Control.Concurrent.STM
import Control.Applicative
import Geordi.Handler
import Geordi.UrlPattern
import Geordi.UrlPattern.Types
import Geordi.TableBuilder
import Data.Text.Lazy hiding (filter, length)
import Control.Monad.Trans.Resource
import qualified Data.Map as M
import Control.Monad.Trans
import Control.Monad.State as S
import Control.Monad.Reader
import Control.Monad.Writer.Class

type SessionId = Int

data SessionDB i d = SDB { newSession  :: forall m. (MonadIO m) => d -> m i
                         , endSession' :: forall m. (MonadIO m) => i -> m ()
                         , withSession :: forall m x. (MonadIO m) => i -> (StateT d m x) -> m x -> m x
                         }


stmSDB :: IO (SessionDB Int d)
stmSDB = do count <- newTVarIO 0
            (sessions :: TVar (M.Map Int d)) <- newTVarIO M.empty
            let new :: (MonadIO m) => d -> m SessionId
                new d = liftIO $ atomically $ do
                          x <- readTVar count
                          modifyTVar sessions (M.insert x d)
                          writeTVar count (x + 1)
                          return x
                end :: (MonadIO m) => Int -> m ()
                end i = liftIO $ atomically $ modifyTVar sessions $ M.delete i
                with :: (MonadIO m) => Int -> (StateT d m x) -> m x -> m x
                with i thn els = do x <- liftM (M.lookup i) $ liftIO (readTVarIO sessions)
                                    case x of Just v -> do (a, v') <- runStateT thn v
                                                           liftIO $ atomically $ modifyTVar sessions (M.adjust (const v') i)
                                                           return a
                                              Nothing -> els
            return $ SDB new end with

newtype SessionT i d m x = SessionT (ReaderT (i , SessionDB i d) (StateT d m) x)
                deriving ( Monad
                         , Functor
                         , Applicative
                         , MonadIO
                         , MonadWriter b
                         , MonadReader (i , SessionDB i d)
                         , MonadState d
                         , MonadUnsafeIO
                         , MonadActive
                         , MonadThrow
                         )
instance MonadTrans (SessionT i d) where
   lift = SessionT . lift . lift

type SessionFunc i d f = (SessionT i d (HandlerM f) HandlerStatus) -> i -> HandlerM f HandlerStatus

session :: forall c i d m p s f x. (Types '[c] ~ '[i], MonadHandler m f, MonadIO m)
        => SessionDB i d -> (forall method. UrlPattern method f '[c])
        -> (TableBuilder (SessionT i d m) p s f x)
        -> TableBuilder m p s f x
session db p a = monadSuffix p (f p) a
  where f :: (forall method. UrlPattern method f '[c])
          -> SessionT i d m HandlerStatus -> i -> m HandlerStatus
        f _ (SessionT x) i = withSession db i (runReaderT x (i , db)) continue

endSession :: (MonadIO m) => SessionT i d m ()
endSession = ask >>= uncurry (flip endSession')

