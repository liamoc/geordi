module Geordi.HandlerTable.Wai where

import qualified Network.Wai as W
import Geordi.HandlerTable
import Geordi.Request.Wai
import Geordi.Response.Wai
import Geordi.FileBackend
import Geordi.Handler

asApplication :: FileBackend f -> HandlerTable (HandlerM f) f -> W.Application
asApplication backend table req = fromWai req backend >>= runTable table >>= return . toWai 
