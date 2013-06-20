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
import Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.Utf8

page = docTypeHtml $ body $ do
         form ! A.action "/foo" ! A.method "post" $ do
           input ! A.name "inpoo" ! A.value "default"
           input ! A.type_ "submit" ! A.value "Foo"

main = geordi 3000 $ do
         post (str "foo" // posted "inpoo") $ \ (x :: T.Text) -> do
           respond $ text x
                   . setCookie ("fook" := x)
                   . status ok200
                   . contentType "text/plain;charset=utf-8"
         request (splat // cookie "fook") $ \(l :: [String]) (r :: T.Text) -> do
           respond $ (text $ r `T.append` "foo")
         request splat $ \(l :: [String]) -> do
           respond $ builder (renderMarkupBuilder page)
                   . status ok200
                   . contentType "text/html; charset=utf-8"
         return ()
