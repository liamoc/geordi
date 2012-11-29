module Geordi.Request where

import qualified Data.Text.Lazy            as T
import qualified Data.Map                  as M
import qualified Network.HTTP.Types.Method as H

import Geordi.FileInfo


data Request f = Request { queries   :: M.Map T.Text [T.Text]
                         , cookies   :: M.Map T.Text [T.Text] 
                         , posts     :: M.Map T.Text [T.Text]
                         , files     :: M.Map T.Text [FileInfo f] 
                         , urlpieces :: [T.Text]
                         , methodStr :: H.Method
                         }


