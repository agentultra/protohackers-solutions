{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Protohacker.Servers.Proto2 where

import Control.Concurrent.STM
import Control.Monad
import Data.Bits.Extras (w32)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Internal (w2c)
import qualified Data.Map.Strict as SM
import Data.Hashable
import Data.Maybe
import Data.Serialize
import Focus (adjust, lookupWithDefault)
import Network.Run.TCP
import Network.Socket
import Network.Socket.ByteString
import StmContainers.Map (Map)
import qualified StmContainers.Map as M

data Insert
  = Insert
  { insertTimestamp :: Int
  , insertAmount    :: Int
  }
  deriving (Eq, Show)

instance Serialize Insert where
  put = error "Not implemented"
  get = do
    tstamp <- fromIntegral <$> getWord32be
    amt <- fromIntegral <$> getWord32be
    pure $ Insert tstamp amt

data Query
  = Query
  { queryMin :: Int
  , queryMax :: Int
  }
  deriving (Eq, Show)

instance Serialize Query where
  put = error "Not implemented"
  get = do
    tmin <- fromIntegral <$> getWord32be
    tmax <- fromIntegral <$> getWord32be
    pure $ Query tmin tmax

data Command
  = CmdI Insert
  | CmdQ Query
  deriving (Eq, Show)

instance Serialize Command where
  put = error "Not implemented"
  get = do
    c <- w2c <$> getWord8
    case c of
      'I' -> CmdI <$> get
      'Q' -> CmdQ <$> get
      _   -> fail "Unrecognized command"

newtype ClientId = ClientId { getClientId :: ByteString }
  deriving stock (Eq, Show)
  deriving newtype (Hashable)

fromSocket :: Socket -> IO ClientId
fromSocket s = ClientId . encode . w32 <$> recvFd s

start :: IO ()
start = do
  clientTickers <- M.newIO
  runTCPServer Nothing "3000" $ tickerServer clientTickers

tickerServer :: Map ClientId (SM.Map Int Int) -> Socket -> IO ()
tickerServer clientTickers s = do
  putStrLn "Received connection..."
  peerAddr <- getPeerName s
  (peerHost, peerService) <-
    getNameInfo [NI_NUMERICHOST, NI_NUMERICSERV] True True peerAddr
  putStrLn $ "Host: " ++ fromMaybe "Unavailable" peerHost
  putStrLn $ "Service: " ++ fromMaybe "Unknown" peerService
  msg <- recv s 1024
  unless (B.null msg) $
    case decode @Command msg of
      Left err -> do
        print err
        pure ()
      Right (CmdI insertion) -> do
        handleInsert clientTickers s insertion
        tickerServer clientTickers s
      Right (CmdQ query) -> do
        answer <- handleQuery clientTickers s query
        sendAll s . encode . w32 $ answer
        tickerServer clientTickers s

handleInsert :: Map ClientId (SM.Map Int Int) -> Socket -> Insert -> IO ()
handleInsert clientTickers s insertion = do
  clientId <- fromSocket s
  atomically $ M.focus (adjust . doInsert $ insertion) clientId clientTickers
  where
    doInsert :: Insert -> SM.Map Int Int -> SM.Map Int Int
    doInsert (Insert tstamp amt) = SM.insert tstamp amt

handleQuery :: Map ClientId (SM.Map Int Int) -> Socket -> Query -> IO Int
handleQuery clientTickers s (Query minT maxT) = do
  clientId <- fromSocket s
  atomically $ do
    clientTicker <- M.focus (lookupWithDefault mempty) clientId clientTickers
    let amts = SM.takeWhileAntitone (\x -> minT <= x && x <= maxT) clientTicker
    pure $ sum amts `div` length amts
