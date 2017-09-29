{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified World
import qualified World.Entity as Entity
import qualified Data.Text as T
import Control.Concurrent (MVar, newMVar, putMVar, takeMVar,
  newEmptyMVar, modifyMVar_, modifyMVar, readMVar, threadDelay, forkIO)
import Control.Exception (finally)
import qualified Control.Exception as E
-- import Control.Concurrent.STM
import qualified Network.WebSockets as WS
import qualified System.Microtimer as MTimer
import Data.Array

-- Representation of a client connected to us - they're more than just the
-- connection.
type Client = (T.Text, WS.Connection)

-- State of the server is a list of clients.
-- type ServerState = (Chunk, [Client])

-- counter :: ServerState -> Int
-- counter sState = fst sState

-- `fastest`: Minimum time difference between IO actions.
throttle :: Double -> [IO a] -> IO [a]
throttle fastest actions = throttle_ fastest actions [] where
  throttle_ :: Double -> [IO a] -> [a] -> IO [a]
  throttle_ fastest [] results = return results
  throttle_ fastest (action:actions) results = do
    (elapsed, x) <- MTimer.time action
    threadDelay $ round $ max (1000000 * (fastest - elapsed)) 0.0
    throttle_ fastest actions (x:results)

main :: IO ()
main = do

  let initialChunk = (World.chunkConst (1,1) 100 World.Wall) //
        [((2,2), World.Space [Entity.Entity 1 1 $ Entity.Blinker Entity.On])]

  -- The state computing buffer - where the most recently computed state is stored.
  stateComputing <- newMVar initialChunk;

  -- The state broadcast buffer - full after the thing has been computed.
  stateBroadcast <- newEmptyMVar :: IO (MVar World.Chunk)

  -- Compute the next local state, writing to the broadcast buffer, and waiting
  -- until the buffer is available to write.

  -- IO Action that computes the next state and updates the buffers.
  let stateStep = do {

    -- Compute the next state.
    putStrLn "Computing next state ...";
    newState <- modifyMVar stateComputing $ return . (\x -> (x,x)) .
      World.boundChunk . World.nextStateMinor;

    -- Update the broadcast buffer, waiting for it to be empty.
    putStrLn "Waiting for the broadcast buffer to be empty ...";
    putMVar stateBroadcast newState;
  }

  -- Run the compute loop in a separate thread.
  -- forkIO $ throttle 1.0 $ repeat $ stateStep
  computeThreadId <- forkIO $ do { throttle 1.0 $ repeat $ stateStep; return () }

  -- Run the WebSocket server in this thread (this will spawn it's own
  -- application threads - one for each connection).
  WS.runServer "127.0.0.1" 9160 $ application stateBroadcast

-- Equivalently, :: MVar ServerState -> WS.PendingConnection -> IO ()
-- or            :: MVar ServerState -> (WS.PendingConnection -> IO ())
application :: MVar World.Chunk -> WS.ServerApp
application worldState pending = do

  -- Accept the connection (TODO: authentication)
  conn <- WS.acceptRequest pending

  -- Keep the WebSocket connection alive by pinging every 30 seconds (this
  -- doesn't interfere with messaging - just ensures the connection doesn't
  -- drop when using proxies and whatever)
  WS.forkPingThread conn 30

  let loop = do {

    -- Wait for any message
    msg <- WS.receiveData conn :: IO T.Text;

    -- Read the world state from the broadcast buffer - if it isn't ready, wait for it.
    chunk <- takeMVar worldState;

    -- Render it and send it.
    WS.sendTextData conn $ T.pack $ World.chunkRender chunk;

    -- Keep looping.
    loop;
  }

  -- Run the loop, handling when a client disconnects.
  flip finally disconnect loop where
    disconnect = do { putStrLn "bye"; }
