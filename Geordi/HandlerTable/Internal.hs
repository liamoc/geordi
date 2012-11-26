{-# LANGUAGE GADTs, KindSignatures, TypeOperators, DataKinds, PolyKinds, LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, OverloadedStrings #-}
module Geordi.HandlerTable.Internal where

import Data.Monoid
import Network.Wai
import Control.Applicative
import Control.Arrow
import Geordi.Handler
import Control.Monad.Trans.Resource
import qualified Network.HTTP.Types.Status as H

data Exists :: (x -> *) -> * where
  ExI :: a b -> Exists a

newtype HandlerTable = HandlerTable [Exists Handler] deriving (Monoid)

singleton :: Handler hs -> HandlerTable
singleton = HandlerTable . (:[]) . ExI

asApplication :: HandlerTable -> Application
asApplication (HandlerTable x) = asApplication' initialResponse x
  where asApplication' :: Response -> [Exists Handler] -> Request -> ResourceT IO Response
        asApplication' res [] _ = return $ res
        asApplication' res (ExI h : hs ) req 
           = matchWai h req >>= \case  
               Nothing  -> asApplication' res hs req
               Just act -> second ($ res) <$> runHandlerM act req >>= \case 
                              (Continue , res') -> asApplication' res' hs req
                              (Done     , res') -> return res' 
        initialResponse = responseLBS H.status500 [] 
                        $  "Captain Picard to the bridge!\n Captain, we've got a problem with the warp core, or the phase inducers --- or some other damn thing.\n\n"
                        <> "                                                  ______                          \n"
                        <> "                                     ___.--------'------`---------.____           \n"
                        <> "                               _.---'----------------------------------`---.__    \n"
                        <> "                             .'___=]===========================================   \n"
                        <> ",-----------------------..__/.'         >--.______        _______.---'            \n"
                        <> "]====================<==||(__)        .'          `------'                        \n"
                        <> "`-----------------------`' ----.___--/                                            \n"
                        <> "     /       /---'                 `/                                             \n"
                        <> "    /_______(______________________/                                              \n"
                        <> "    `-------------.--------------.'                                               \n"
                        <> "                   \\________|_.-'                                                \n"
                        <> "                                                                                  \n"
                        <> " (Most likely, the developer hasn't added a `catch-all' 404 handler... yet)       \n"
