{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_) -- from async
import Control.Concurrent.STM -- from stm
import Control.Concurrent.STM.TMChan -- from stm-chans
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON) -- from aeson
import Data.Binary.Builder
import Data.Foldable (for_)
import Data.Text (Text)
import Network.Wai.EventSource
import Network.Wai.Middleware.AddHeaders
import Network.Wai.Application.Static
import Yesod
import Network.Wai.Handler.Warp


data HelloWorld = HelloWorld (TMChan ServerEvent)

mkYesod
  "HelloWorld"
  [parseRoutes|
/foo HomeR GET
|]

instance Yesod HelloWorld

--foof :: IO ServerEvent

getHomeR :: Handler ()
getHomeR = do
  --addHeader "Access-Control-Allow-Origin:" "*"
  HelloWorld chan <- getYesod
  --sendWaiApplication . addHeaders [("Access-Control-Allow-Origin", "http://localhost:3001")] . eventStreamAppRaw $ \send flush ->
  sendWaiApplication . eventStreamAppRaw $ \send flush ->
    let go = do
          mevent <- liftIO $ atomically $ readTMChan chan
          case mevent of
            Nothing -> do
              send CloseEvent
              flush
            Just event -> do
              send event
              flush
              go
     in go


-- getAnotherR :: Handler Value
-- getAnotherR = do
--     sendWaiApplication $ staticApp $ defaultWebAppSettings "."

main :: IO ()
main =
  do
    chan <- atomically $ newTMChan @ServerEvent
    concurrently_
      ( do
          for_
            [ 
             ServerEvent (Just (fromByteString "starting")) (Just (fromByteString "aa1")) [fromByteString "payload1"],
             ServerEvent (Just (fromByteString "workingonit")) (Just (fromByteString "aa2")) [fromByteString "payload2"],
             ServerEvent (Just (fromByteString "finishing")) (Just (fromByteString "aa3")) [fromByteString "payload3"]
            ]
            ( \msg -> do
                threadDelay 10e6
                atomically $ writeTMChan chan msg
            )
          atomically $ closeTMChan chan
      )
      ( warp 3000 (HelloWorld chan)
      )
