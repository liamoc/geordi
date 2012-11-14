{-# LANGUAGE QuasiQuotes, GADTs, ScopedTypeVariables, RecursiveDo, OverloadedStrings #-}

import FakeWebFramework
import UrlPath
import Data.IORef
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Monad.Trans
import Network.Wai.Handler.Warp
import Network.HTTP.Types.Status
import Network.Wai
main :: IO ()
main = do
  counter <- newIORef 0
  run 3000 =<< (notActuallyAWebFramework $ prefix [r| /admin |] $ mdo
    let showCounter = do x <- readIORef counter 
                         return $ responseLBS status200 [] $ BL.pack $  ("The counter is:" ++) $ show x
    increment <- get [r| /count/increment |] $ liftIO $do 
      modifyIORef counter (1 +)                      
      putStrLn $ "Go to " ++ T.unpack (link decrement) ++  " to undo this"
      showCounter
    decrement <- get [r| /count/decrement |] $ liftIO $do
      modifyIORef counter (\x -> x - 1)
      putStrLn $ "Go to " ++ T.unpack (link increment) ++ " to undo this"
      showCounter 
    getOrSet <- get [r| /count/getorset?.val |] $ \ (x :: Maybe Int) -> liftIO $ do
      case x of Just x -> modifyIORef counter (const x); Nothing -> return ()
      showCounter
    setAdd <- get [r| /count/setadd?first&.second |] $ \ (y :: Int) (x :: Maybe Int) -> liftIO $ do
      modifyIORef counter (const $ y  + fromMaybe 0 x)
      showCounter
    setCounter <- prefix [r| /count/set?val |] $ mdo
      setCounter <- get [r| / |] $ \ (x :: Int) -> liftIO $ do
        writeIORef counter x
        putStrLn $ "Go to " ++ T.unpack (link setSecret x False) ++ " for more information!"
        showCounter
      setSecret <- suffix ([r| /:secret |] ) $ get [r| / |] $ \ (x :: Int) (secret :: Bool) -> liftIO $do 
          if secret then putStrLn $ "The secret is 42! Now you know, you may as well just use " ++ T.unpack (link setCounter x) ++ " from now on."
                    else putStrLn $ "Not telling the secret. Try going to " ++ T.unpack (link setSecret x True) ++ " for the secret!"
          writeIORef counter x
          showCounter
      return (setCounter)    
    return ())
              


