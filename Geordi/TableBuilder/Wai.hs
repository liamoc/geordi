{-# LANGUAGE DataKinds #-}
module Geordi.TableBuilder.Wai where

import qualified Network.Wai as W

import Geordi.FileBackend
import Geordi.TableBuilder
import Geordi.HandlerTable.Wai

buildApplication :: FileBackend f -> TableBuilder '[] '[] f () -> W.Application
buildApplication b = asApplication b . snd . buildTable
