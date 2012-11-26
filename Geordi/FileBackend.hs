module Geordi.FileBackend where
import qualified Data.ByteString.Lazy as L
import Network.Wai.Parse
import Data.Conduit.Util

newtype FileBackend a = FB (BackEnd a)

nullBackend :: FileBackend a
nullBackend = FB $ const $ const $ sinkState () 
                         (\() _ -> return $ StateDone Nothing (error "Using undefined file backend")) 
                         (const $ return $ error "Using undefined file backend") 

lbsBackend :: FileBackend L.ByteString
lbsBackend = FB $ lbsBackEnd

tempFileBackend :: FileBackend FilePath
tempFileBackend = FB $ tempFileBackEnd
