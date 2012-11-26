{-# LANGUAGE QuasiQuotes, GADTs, ScopedTypeVariables, RecursiveDo, OverloadedStrings #-}

import Geordi
import Network.Wai.Handler.Warp
import Debug.Trace
import Data.Monoid

main = geordi 3000 $ return ()
