{-# LANGUAGE QuasiQuotes, GADTs, ScopedTypeVariables, RecursiveDo, OverloadedStrings #-}

import Geordi
import Network.Wai.Handler.Warp
import Debug.Trace
import Data.Monoid
import Network.HTTP.Types.Status
import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text.Lazy as T
import qualified Data.Map  as M
main = geordi 3000 $ do
         get (str "baz" // splat // str "foo" // splat // str "bing") $ \(l :: [String]) (r :: [String]) -> do
           respond $ text (T.pack $ concat l)
                   . status ok200
                   . contentType "text/html; charset=utf-8"
         get (str "baz" // splat // str "bar" // allQuery) $ \(l :: [String]) (qs :: M.Map T.Text [String]) -> do
           respond id
         request splat $ \(l :: [String]) -> do
           respond id

         return ()
