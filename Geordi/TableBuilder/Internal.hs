{-# LANGUAGE GeneralizedNewtypeDeriving, DataKinds, GADTs, RankNTypes, KindSignatures, TypeOperators, OverloadedStrings #-}
module Geordi.TableBuilder.Internal where

import Geordi.HandlerTable
import Geordi.UrlPattern
import Geordi.Handler

import Control.Arrow
import Control.Monad.Reader 
import Control.Monad.Writer 
import Control.Applicative

data GenUrlPattern (f :: * ) (x :: [SegmentType *]) where 
  Generalise :: (forall m. UrlPattern m f x) -> GenUrlPattern f x

interpret :: GenUrlPattern f x -> UrlPattern m f x
interpret (Generalise x) = x

newtype TableBuilder p s f x = TableBuilder (ReaderT (GenUrlPattern f p, GenUrlPattern f s) (Writer (HandlerTable f)) x)  
  deriving ( Functor
           , Monad
           , Applicative
           , MonadWriter (HandlerTable f)
           , MonadFix
           )

prefix :: (forall m. UrlPattern m f n) -> TableBuilder (p :++: n) s f x -> TableBuilder p s f x
prefix p (TableBuilder x) 
  = do (Generalise v, s) <- TableBuilder ask
       let (r,w) = runWriter $ flip runReaderT (Generalise (v // p), s) x
       tell w
       return r

suffix :: (forall m. UrlPattern m f n) -> TableBuilder p (s :++: n) f x -> TableBuilder p s f x
suffix p (TableBuilder x) 
  = do (px, Generalise v) <- TableBuilder ask
       let (r, w) = runWriter $ flip runReaderT (px, Generalise (v // p)) x
       tell w
       return r

add :: Handler f u -> TableBuilder p s f ()
add = tell . singleton

-- | Adds a handler to the table and returns it
handle :: MethodSingleton m -- ^ 'MethodGet' or 'MethodPost', indicating the HTTP method this handler will require
       -> UrlPattern m f ts -- ^ A 'UrlPattern' for this handler to check requests against
       -> (Types ((prefix :++: ts) :++: suffix) :--> HandlerM f HandlerStatus) -- ^ An action to execute if the pattern matches
       -> TableBuilder prefix suffix f (Handler f ((prefix :++: ts) :++: suffix)) -- ^ Action that returns the handler and adds it to the table
handle m p a
  = do (px, sx) <- (interpret *** interpret) 
               <$> (TableBuilder ask :: TableBuilder prefix suffix f (GenUrlPattern f prefix, GenUrlPattern f suffix))
       let pattern = ((px // p) // sx)  
       let x = Handler m pattern a
       add x
       return x

-- | Simply 'handle' 'MethodGet'
get :: UrlPattern GET f ts -- ^ Url pattern
    -> (Types ((prefix :++: ts) :++: suffix) :--> HandlerM f HandlerStatus) -- ^ Handler action
    -> TableBuilder prefix suffix f (Handler f ((prefix :++: ts) :++: suffix))
get  = handle MethodGet
-- | Simply 'handle' 'MethodPost'
post :: UrlPattern POST f ts -- ^ Url pattern 
     -> (Types ((prefix :++: ts) :++: suffix) :--> HandlerM f HandlerStatus) -- ^ Handler action
     -> TableBuilder prefix suffix f (Handler f ((prefix :++: ts) :++: suffix))
post = handle MethodPost
-- | Simply 'handle' 'MethodAny'
request :: UrlPattern AnyMethod f ts -- ^ Url pattern
        -> (Types ((prefix :++: ts) :++: suffix) :--> HandlerM f HandlerStatus) -- ^ Handler action
        -> TableBuilder prefix suffix f (Handler f ((prefix :++: ts) :++: suffix))
request = handle MethodAny

buildTable :: TableBuilder '[] '[] f r -> (r, HandlerTable f)
buildTable (TableBuilder x) = runWriter
                            . flip runReaderT (Generalise nil , Generalise nil ) 
                            $ x

