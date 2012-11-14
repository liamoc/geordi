{-# LANGUAGE GADTs, KindSignatures, TypeFamilies, TypeOperators, DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, RankNTypes, OverloadedStrings #-}
module Geordi where

import Geordi.UrlPath

import Control.Applicative
import Control.Arrow
import Control.Monad.Reader 
import Control.Monad.Writer 
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T

import Data.Conduit
import Network.HTTP.Types.Status
import Network.HTTP.Types.URI
import Network.Wai

data GenUrlPath (x :: [SegmentType *]) where 
  Generalise :: (forall m. UrlPath m x) -> GenUrlPath x

newtype HandlerTable p s x = HT { unHT :: ReaderT (GenUrlPath p, GenUrlPath s) 
                                        ( WriterT [ExistsSegs Handler] IO) x  
                                } 
  deriving ( Functor
           , Monad
           , Applicative
           , MonadIO
           , MonadWriter [ExistsSegs Handler]
           , MonadReader (GenUrlPath p, GenUrlPath s)
           , MonadFix
           )

type HandlerM x = ResourceT IO x

type family   HandlerFor (ts :: [SegmentType *])
type instance HandlerFor ('[])                 = HandlerM Response
type instance HandlerFor (StringSeg ': xs)     = HandlerFor xs
type instance HandlerFor (UrlParam x ': xs)    = x -> HandlerFor xs
type instance HandlerFor (QueryParam x ': xs)  = x -> HandlerFor xs
type instance HandlerFor (PostParam x ': xs)   = x -> HandlerFor xs
type instance HandlerFor (CookieParam x ': xs) = x -> HandlerFor xs

data ExistsSegs :: ([SegmentType *] -> *) -> * where
  ExI :: a b -> ExistsSegs a

data Handler :: [SegmentType *] -> * where
  Handle :: UrlPath m ts -> HandlerFor ts -> Handler ts

runHandler :: (T.Text -> Maybe T.Text) 
           -> [T.Text] 
           -> UrlPath m ts 
           -> HandlerFor ts 
           -> Maybe (HandlerM Response)
runHandler t (x:xs) (Str x'     :/ ps) h = if x == x' then runHandler t xs ps h
                                                      else Nothing
runHandler t (x:xs) (Param      :/ ps) h = parse x 
                                           >>= runHandler t xs ps . h 
runHandler t xs     (Query x    :/ ps) h = t x >>= parse
                                           >>= runHandler t xs ps . h 
runHandler t xs     (QueryOpt x :/ ps) h = maybe (Just Nothing) (fmap Just . parse) (t x) 
                                           >>= runHandler t xs ps . h 
runHandler t []     (Empty           ) h = Just h 
runHandler _ _      _                  _ = Nothing

go :: [ExistsSegs Handler] -> Application
go [] _ = do liftIO $ putStrLn "404 not found"; return $ responseLBS status404 [] "404 not found"
go (ExI (Handle a b):hs) req 
  = let queries = flip lookup $ map (second $ fromMaybe "") $ queryToQueryText $ queryString req
     in case runHandler queries (pathInfo req) a b of 
          Just v -> v
          Nothing -> go hs req

add :: Handler u -> HandlerTable p s ()
add x = tell [ExI x]

interpret :: GenUrlPath x -> UrlPath m x
interpret (Generalise x) = x

handle :: MethodSingleton m 
       -> UrlPath m ts 
       -> HandlerFor ((prefix :++: ts) :++: suffix) 
       -> HandlerTable prefix suffix (Handler ((prefix :++: ts) :++: suffix))
handle method path action
  = do (px, sx) <- (interpret *** interpret) 
               <$> (ask :: HandlerTable prefix suffix (GenUrlPath prefix, GenUrlPath suffix))
       let x = Handle ((px // path) // sx) action 
       add x
       return x

get  = handle MethodGet
post = handle MethodPost

link :: Handler ts -> LinkTo ts
link (Handle u _) = linkUrl u

notActuallyAWebFramework :: HandlerTable '[] '[] () -> IO (Application)
notActuallyAWebFramework = fmap go 
                         . fmap snd 
                         . runWriterT 
                         . flip runReaderT (Generalise Empty , Generalise Empty) 
                         . unHT

prefix :: (forall m. UrlPath m n) -> HandlerTable (p :++: n) s x -> HandlerTable p s x
prefix p (HT x) = do (Generalise v, s) <- ask
                     (r, w) <- liftIO $ runWriterT $ flip runReaderT (Generalise (v // p), s) x
                     tell w
                     return r

suffix :: (forall m. UrlPath m n) -> HandlerTable p (s :++: n) x -> HandlerTable p s x
suffix p (HT x) = do (px, Generalise v) <- ask
                     (r, w) <- liftIO $ runWriterT $ flip runReaderT (px, Generalise (v // p)) x
                     tell w
                     return r

