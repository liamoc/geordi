{-# LANGUAGE TypeOperators, QuasiQuotes, GADTs, ScopedTypeVariables, RecursiveDo, OverloadedStrings #-}

import Geordi
import Network.Wai.Handler.Warp
import Debug.Trace
import Data.Monoid
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text.Lazy as T
import qualified Data.Map  as M
import Text.Blaze.Renderer.Utf8
import Text.Hamlet
import Control.Concurrent.STM
import SessionDB as Session
import qualified Control.Monad.State as S

pageWithSession logout x = [shamlet|
   !!!
   <body>
     <form action="#{link logout}" method="post">
       Count: #{x}
       <input type="submit" value="Logout">
   |]
pageWithoutSession login = [shamlet|
   !!!
   <body>
     <form action="#{link login}" method="post">
       You Have no Session
       <input type="submit" value="Get one">
   |]

pageLogout home = [shamlet|
   !!!
   <body>
     You are now logged out
     <a href="#{link home}">
       Go back
   |]

html x = builder (renderMarkupBuilder x)
       . status ok200
       . contentType "text/html; charset=utf-8"

redirect' :: T.Text -> Response -> Response
redirect' x = status found302
           . header hLocation x

redirect :: Handler m f ts -> Types (LinkSegments ts) :--> (Response -> Response)
redirect h@(Handler {urlPat = urlPat}) = mapMany (linkWitness urlPat) redirect' (link h)

main = do db <- stmSDB
          geordi 3000 $ mdo
            inward <- session db (cookie "session") $ do
              logout <- post (str "session") $ do
                endSession
                respond $ redirect home
              request (nil) $ do
                S.modify (+1)
                (v :: Int) <- S.get
                respond $ html $ pageWithSession logout v
            login <- post (str "session") $ do
              id <- newSession db 0
              respond $ redirect inward
                      . setCookie ("session" := T.pack (show id))
            home <- request nil $
              respond $ html $ pageWithoutSession login
            return ()
