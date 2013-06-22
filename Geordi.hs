{-# LANGUAGE DataKinds #-}
module Geordi ( module Geordi.UrlPattern
              , module Geordi.HandlerTable
              , module Geordi.Handler
              , module Geordi.TableBuilder
              , module Geordi.Response
              , geordi
              ) where

import Geordi.UrlPattern
import Geordi.HandlerTable
import Geordi.Handler
import Geordi.TableBuilder
import Geordi.FileBackend
import Geordi.TableBuilder.Wai
import Geordi.Response
import Network.Wai.Handler.Warp

geordi :: Int -> TableBuilder (HandlerM FilePath) '[] '[] FilePath () ->  IO ()
geordi p = run p . buildApplication tempFileBackend
