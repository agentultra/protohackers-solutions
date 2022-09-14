module Protohacker.Servers.Smoke where

import Control.Monad
import qualified Data.ByteString as B
import Network.Socket.ByteString
import Network.Run.TCP

start :: IO ()
start = runTCPServer Nothing "3000" echo
  where
    echo s = do
      msg <- recv s 1024
      unless (B.null msg) $ do
        sendAll s msg
        echo s
