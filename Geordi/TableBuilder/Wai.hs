{-# LANGUAGE DataKinds #-}
module Geordi.TableBuilder.Wai where

import qualified Network.Wai as W

import Geordi.FileBackend
import Geordi.TableBuilder
import Geordi.HandlerTable.Wai
import Geordi.Handler

buildApplication :: FileBackend f -> TableBuilder (HandlerM f) '[] '[] f () -> W.Application
buildApplication b = asApplication b . snd . buildTable
