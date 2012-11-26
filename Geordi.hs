{-# LANGUAGE DataKinds #-}
module Geordi ( module Geordi.UrlPattern
              , module Geordi.HandlerTable
              , module Geordi.Handler
              , module Geordi.TableBuilder
              , geordi
              ) where

import Geordi.UrlPattern
import Geordi.HandlerTable
import Geordi.Handler
import Geordi.TableBuilder

import Network.Wai.Handler.Warp

geordi :: Int -> TableBuilder '[] '[] () ->  IO ()
geordi p = run p . buildApplication
