{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_) -- from async
import Control.Concurrent.STM -- from stm
import Control.Concurrent.STM.TMChan -- from stm-chans
import Control.Monad.IO.Class (liftIO)
import Data.Binary.Builder --from binay
import Data.Foldable (for_)
import Network.Wai.EventSource -- from wai-extra
import Network.Wai.Handler.Warp -- from warp
import Network.Wai.Middleware.AddHeaders

main :: IO ()
main =
  do
    chan <- atomically $ newTMChan
    concurrently_
      ( do
          for_
            [ ServerEvent (Just . fromByteString $ "noty")    (Just . fromByteString $ "aa1") [fromByteString "payload1"],
              ServerEvent (Just . fromByteString $ "noty") (Just . fromByteString $ "aa2") [fromByteString "payload2"],
              ServerEvent (Just . fromByteString $ "noty")   (Just . fromByteString $ "aa3") [fromByteString "payload3"],
              ServerEvent (Just . fromByteString $ "eof")   (Just . fromByteString $ "theend") [fromByteString "theend"]
            ]
            ( \msg -> do
                threadDelay 10e6
                atomically $ writeTMChan chan msg
            )
          atomically $ closeTMChan chan
      )
      ( run 3000
          $ addHeaders [("Access-Control-Allow-Origin", "*")]
          $ eventStreamAppRaw
          $ \send flush ->
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
      )
