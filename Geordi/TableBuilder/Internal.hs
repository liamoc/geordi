{-# LANGUAGE GeneralizedNewtypeDeriving, DataKinds, GADTs, RankNTypes, KindSignatures, TypeOperators, OverloadedStrings #-}
module Geordi.TableBuilder.Internal where

import Geordi.HandlerTable
import Geordi.UrlPattern
import Geordi.Handler
import Geordi.FileBackend

import Network.Wai

import Control.Arrow
import Control.Monad.Reader 
import Control.Monad.Writer 
import Control.Applicative

data GenUrlPattern (x :: [SegmentType *]) where 
  Generalise :: (forall m f. UrlPattern m f x) -> GenUrlPattern x

interpret :: GenUrlPattern x -> UrlPattern m f x
interpret (Generalise x) = x

newtype TableBuilder p s x = TableBuilder (ReaderT (GenUrlPattern p, GenUrlPattern s) (Writer HandlerTable) x)  
  deriving ( Functor
           , Monad
           , Applicative
           , MonadWriter HandlerTable
           , MonadFix
           )

prefix :: (forall m f. UrlPattern m f n) -> TableBuilder (p :++: n) s x -> TableBuilder p s x
prefix p (TableBuilder x) 
  = do (Generalise v, s) <- TableBuilder ask
       let (r,w) = runWriter $ flip runReaderT (Generalise (v // p), s) x
       tell w
       return r

suffix :: (forall m f. UrlPattern m f n) -> TableBuilder p (s :++: n) x -> TableBuilder p s x
suffix p (TableBuilder x) 
  = do (px, Generalise v) <- TableBuilder ask
       let (r, w) = runWriter $ flip runReaderT (px, Generalise (v // p)) x
       tell w
       return r

add :: Handler u -> TableBuilder p s ()
add = tell . singleton

-- | Adds a handler to the table and returns it
handle :: MethodSingleton m -- ^ 'MethodGet' or 'MethodPost', indicating the HTTP method this handler will require
       -> UrlPattern m f ts -- ^ A 'UrlPattern' for this handler to check requests against
       -> (Types ((prefix :++: ts) :++: suffix) :--> HandlerM HandlerStatus) -- ^ An action to execute if the pattern matches
       -> TableBuilder prefix suffix (Handler ((prefix :++: ts) :++: suffix)) -- ^ Action that returns the handler and adds it to the table
handle m p a
  = do (px, sx) <- (interpret *** interpret) 
               <$> (TableBuilder ask :: TableBuilder prefix suffix (GenUrlPattern prefix, GenUrlPattern suffix))
       let pattern = ((px // p) // sx)  
       let x = Handler m (fileBackend lbsBackend tempFileBackend nullBackend pattern) pattern a
       add x
       return x

-- | Simply 'handle' 'MethodGet'
get :: UrlPattern GET f ts -- ^ Url pattern
    -> (Types ((prefix :++: ts) :++: suffix) :--> HandlerM HandlerStatus) -- ^ Handler action
    -> TableBuilder prefix suffix (Handler ((prefix :++: ts) :++: suffix))
get  = handle MethodGet
-- | Simply 'handle' 'MethodPost'
post :: UrlPattern POST f ts -- ^ Url pattern 
     -> (Types ((prefix :++: ts) :++: suffix) :--> HandlerM HandlerStatus) -- ^ Handler action
     -> TableBuilder prefix suffix (Handler ((prefix :++: ts) :++: suffix))
post = handle MethodPost

buildTable :: TableBuilder '[] '[] r -> (r, HandlerTable)
buildTable (TableBuilder x) = runWriter
                            . flip runReaderT (Generalise nil , Generalise nil ) 
                            $ x

buildApplication :: TableBuilder '[] '[] () -> Application
buildApplication = asApplication . snd . buildTable
