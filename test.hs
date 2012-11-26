{-# LANGUAGE QuasiQuotes, GADTs, ScopedTypeVariables, RecursiveDo, OverloadedStrings #-}

import Geordi
import Network.Wai.Handler.Warp
import Debug.Trace
import Data.Monoid
import Network.HTTP.Types.Status
import Control.Monad.Trans
import Network.Wai
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import qualified Data.Map  as M
main = geordi 3000 $ do
         get (str "baz" // splat // str "foo" // splat // str "bing") $ \l r -> do
           liftIO $ putStrLn $ "1: " ++ (concat l)
           liftIO $ putStrLn $ "2: " ++ (concat r)
           respond $ const $ responseLBS status404 [] $ L.pack (concat l)
         get (str "baz" // splat // str "bar" // allQuery) $ \l (qs :: M.Map T.Text [String]) -> do
           liftIO $ putStrLn $ show qs
           respond $ const $ responseLBS status404 [] $ L.pack (concat l)
         request splat $ \l -> do
           liftIO $ putStrLn (concat l)
           respond $ const $ responseLBS status404 [] "FOOBAR"
         return ()
