module Geordi.FileBackend where
import qualified Data.ByteString.Lazy as L
import Network.Wai.Parse

newtype FileBackend a = FB (BackEnd a)

lbsBackend :: FileBackend L.ByteString
lbsBackend = FB $ lbsBackEnd

tempFileBackend :: FileBackend FilePath
tempFileBackend = FB $ tempFileBackEnd
