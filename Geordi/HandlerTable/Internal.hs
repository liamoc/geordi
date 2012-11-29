{-# LANGUAGE GADTs, KindSignatures, TypeOperators, DataKinds, PolyKinds, LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, OverloadedStrings #-}
module Geordi.HandlerTable.Internal where
import qualified Data.Text.Lazy as T
import Data.Monoid
import Control.Applicative
import Control.Arrow
import Geordi.Handler
import Control.Monad.Trans.Resource
import Geordi.Util.Exists
import Geordi.Request
import Geordi.Response

newtype HandlerTable f = HandlerTable [Exists (Handler f)] deriving (Monoid)

singleton :: Handler f hs -> HandlerTable f
singleton = HandlerTable . (:[]) . ExI

runTable :: HandlerTable f -> Request f -> ResourceT IO Response
runTable (HandlerTable x) = go initialResponse x
  where go :: Response -> [Exists (Handler f)] -> Request f -> ResourceT IO Response
        go res [] _ = return $ res
        go res (ExI h : hs ) req 
           = case matchRequest h req of
               Nothing  -> go res hs req
               Just act -> second ($ res) <$> runHandlerM act req >>= \case 
                              (Continue , res') -> go res' hs req
                              (Done     , res') -> return res' 
        initialResponse = text ( 
                          "Captain Picard to the bridge!\n Captain, we've got a problem with the warp core, or the phase inducers --- or some other damn thing.\n\n"
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
                        ) emptyResponse 
