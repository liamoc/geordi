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
import qualified Data.Text.Encoding as E
import Data.Conduit
import Data.Conduit.Util
import Network.HTTP.Types.Status
import Network.HTTP.Types.URI
import Network.HTTP.Types.Method
import Network.HTTP.Types.Header
import Network.Wai
import Network.Wai.Parse
import Web.Cookie
data GenUrlPath (x :: [SegmentType *]) where 
  Generalise :: (forall m f. UrlPath m f x) -> GenUrlPath x

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
type instance HandlerFor (FileParam x ': xs)   = x -> HandlerFor xs

data ExistsSegs :: ([SegmentType *] -> *) -> * where
  ExI :: a b -> ExistsSegs a

data Handler :: [SegmentType *] -> * where
  Handle :: UrlPath m f ts -> MethodSingleton m -> HandlerFor ts -> Handler ts

getBackend :: UrlPath m f ts -> BackEnd f
getBackend (Empty) = const $ const $ sinkState () (\() _ -> return $ StateDone Nothing (error "Using undefined file backend")) (const $ return $ error "Using undefined file backend") 
getBackend (File x :/ r) = tempFileBackEnd
getBackend (Stream x :/ r) = lbsBackEnd
getBackend (p :/ ps) = getBackend ps 

runHandler :: (T.Text -> Maybe T.Text) 
           -> (T.Text -> Maybe T.Text) 
           -> (T.Text -> Maybe T.Text) 
           -> [T.Text] 
           -> UrlPath m f ts 
           -> HandlerFor ts 
           -> Maybe (HandlerM Response)
runHandler q p c (x:xs) (Str x'     :/ ps) h = if x == x' then runHandler q p c xs ps h
                                                      else Nothing
runHandler q p c (x:xs) (Param      :/ ps) h = parse x 
                                           >>= runHandler q p c xs ps . h 
runHandler q p c xs     (Query x    :/ ps) h = q x >>= parse
                                           >>= runHandler q p c xs ps . h 
runHandler q p c xs     (QueryOpt x :/ ps) h = maybe (Just Nothing) (fmap Just . parse) (q x) 
                                           >>= runHandler q p c xs ps . h 
runHandler q p c xs     (Posted x    :/ ps) h = p x >>= parse
                                           >>= runHandler q p c xs ps . h 
runHandler q p c xs     (PostedOpt x :/ ps) h = maybe (Just Nothing) (fmap Just . parse) (p x) 
                                           >>= runHandler q p c xs ps . h 
runHandler q p c xs     (Cookie x    :/ ps) h = c x >>= parse
                                           >>= runHandler q p c xs ps . h 
runHandler q p c xs     (CookieOpt x :/ ps) h = maybe (Just Nothing) (fmap Just . parse) (c x) 
                                           >>= runHandler q p c xs ps . h 
runHandler q p c []     (Empty           ) h = Just h 
runHandler _ _ _ _      _                  _ = Nothing

go :: [ExistsSegs Handler] -> Application
go [] _ = do liftIO $ putStrLn "404 not found"; return $ responseLBS status404 [] "404 not found"
go (ExI (Handle a m b):hs) req 
  = let queries = flip lookup $ map (second $ fromMaybe "") $ queryToQueryText $ queryString req
        cookies = case lookup hCookie $ requestHeaders $ req of
                     Just str -> flip lookup $ parseCookiesText str
                     Nothing  -> const Nothing
     in do posted <- if requestMethod req == methodPost
                      then flip lookup . map (E.decodeUtf8 *** E.decodeUtf8) . fst <$> parseRequestBody (getBackend a) req 
                      else return $ const Nothing
           case (if methodString m == requestMethod req then runHandler queries posted cookies (pathInfo req) a b else Nothing) of 
             Just v -> v
             Nothing -> go hs req
 where methodString MethodGet = methodGet
       methodString MethodPost = methodPost

add :: Handler u -> HandlerTable p s ()
add x = tell [ExI x]

interpret :: GenUrlPath x -> UrlPath m f x
interpret (Generalise x) = x

handle :: MethodSingleton m 
       -> UrlPath m f ts 
       -> HandlerFor ((prefix :++: ts) :++: suffix) 
       -> HandlerTable prefix suffix (Handler ((prefix :++: ts) :++: suffix))
handle method path action
  = do (px, sx) <- (interpret *** interpret) 
               <$> (ask :: HandlerTable prefix suffix (GenUrlPath prefix, GenUrlPath suffix))
       let x = Handle ((px // path) // sx) method action 
       add x
       return x

get  = handle MethodGet
post = handle MethodPost

link :: Handler ts -> LinkTo ts
link (Handle u _ _) = linkUrl u

geordi :: HandlerTable '[] '[] () -> IO (Application)
geordi = fmap go 
       . fmap snd 
       . runWriterT 
       . flip runReaderT (Generalise Empty , Generalise Empty) 
       . unHT

prefix :: (forall m f. UrlPath m f n) -> HandlerTable (p :++: n) s x -> HandlerTable p s x
prefix p (HT x) = do (Generalise v, s) <- ask
                     (r, w) <- liftIO $ runWriterT $ flip runReaderT (Generalise (v // p), s) x
                     tell w
                     return r

suffix :: (forall m f. UrlPath m f n) -> HandlerTable p (s :++: n) x -> HandlerTable p s x
suffix p (HT x) = do (px, Generalise v) <- ask
                     (r, w) <- liftIO $ runWriterT $ flip runReaderT (px, Generalise (v // p)) x
                     tell w
                     return r

