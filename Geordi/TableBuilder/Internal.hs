{-# LANGUAGE GeneralizedNewtypeDeriving, DataKinds, GADTs, RankNTypes, KindSignatures, TypeOperators, OverloadedStrings,ScopedTypeVariables #-}
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

newtype TableBuilder m p s f x = TableBuilder (ReaderT (GenUrlPattern f p, GenUrlPattern f s) (Writer (HandlerTable m f)) x)
  deriving ( Functor
           , Monad
           , Applicative
           , MonadWriter (HandlerTable m f)
           , MonadFix
           )

prefix :: (forall method. UrlPattern method f n) -> TableBuilder m (p :++: n) s f x -> TableBuilder m p s f x
prefix p (TableBuilder x)
  = do (Generalise v, s) <- TableBuilder ask
       let (r,w) = runWriter $ flip runReaderT (Generalise (v // p), s) x
       tell w
       return r

suffix :: (forall method. UrlPattern method f n) -> TableBuilder m p (s :++: n) f x -> TableBuilder m p s f x
suffix p (TableBuilder x)
  = do (px, Generalise v) <- TableBuilder ask
       let (r, w) = runWriter $ flip runReaderT (px, Generalise (v // p)) x
       tell w
       return r

add :: Handler m f u -> TableBuilder m p s f ()
add = tell . singleton

monadSuffix :: forall m2 m f n p s x. (MonadHandler m2 f, MonadHandler m f) 
            => (forall method. UrlPattern method f n)
            -> (m HandlerStatus -> Types n :--> m2 HandlerStatus)
            -> TableBuilder m p s f x
            -> TableBuilder m2 p s f x
monadSuffix p ml (TableBuilder rtw) = let f :: Handler m f ts -> Handler m2 f (ts :++: n)
                                          f  = monadSuffixH p ml
                                          g :: HandlerTable m f -> HandlerTable m2 f
                                          g  = monadSuffixTable p f
                                       in TableBuilder $ do x <- ask
                                                            lift $ mapWriter (second g) (runReaderT rtw x)
-- | Adds a handler to the table and returns it
handle :: (MonadHandler m f)
       => MethodSingleton method -- ^ 'MethodGet' or 'MethodPost', indicating the HTTP method this handler will require
       -> UrlPattern method f ts -- ^ A 'UrlPattern' for this handler to check requests against
       -> (Types ((prefix :++: ts) :++: suffix) :--> m HandlerStatus) -- ^ An action to execute if the pattern matches
       -> TableBuilder m prefix suffix f (Handler m f ((prefix :++: ts) :++: suffix)) -- ^ Action that returns the handler and adds it to the table
handle m p a
  = do (px, sx) <- (interpret *** interpret)
               <$> (TableBuilder ask :: TableBuilder m prefix suffix f (GenUrlPattern f prefix, GenUrlPattern f suffix))
       let pattern = ((px // p) // sx)
       let x = Handler m pattern a
       add x
       return x


-- | Simply 'handle' 'MethodGet'
get :: (MonadHandler m f)
    => UrlPattern GET f ts -- ^ Url pattern
    -> (Types ((prefix :++: ts) :++: suffix) :--> m HandlerStatus) -- ^ Handler action
    -> TableBuilder m prefix suffix f (Handler m f ((prefix :++: ts) :++: suffix))
get  = handle MethodGet
-- | Simply 'handle' 'MethodPost'
post :: (MonadHandler m f)
     => UrlPattern POST f ts -- ^ Url pattern
     -> (Types ((prefix :++: ts) :++: suffix) :--> m HandlerStatus) -- ^ Handler action
     -> TableBuilder m prefix suffix f (Handler m f ((prefix :++: ts) :++: suffix))
post = handle MethodPost
-- | Simply 'handle' 'MethodAny'
request :: (MonadHandler m f)
        => UrlPattern AnyMethod f ts -- ^ Url pattern
        -> (Types ((prefix :++: ts) :++: suffix) :--> m HandlerStatus) -- ^ Handler action
        -> TableBuilder m prefix suffix f (Handler m f ((prefix :++: ts) :++: suffix))
request = handle MethodAny

buildTable :: TableBuilder m '[] '[] f r -> (r, HandlerTable m f)
buildTable (TableBuilder x) = runWriter
                            . flip runReaderT (Generalise nil , Generalise nil )
                            $ x

