module Geordi.HandlerTable.Wai where

import qualified Network.Wai as W
import Geordi.HandlerTable
import Geordi.Request.Wai
import Geordi.Response.Wai
import Geordi.FileBackend

asApplication :: FileBackend f -> HandlerTable f -> W.Application
asApplication backend table req = fromWai req backend >>= runTable table >>= return . toWai 
