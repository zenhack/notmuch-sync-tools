{-# LANGUAGE RecordWildCards #-}
module Rpc where
-- Simple RPC module.
--
-- This is WIP, but the plan is:
--
-- * Use ssh to spawn a server implemented as part of this tool.
-- * Talk to it via stdio
-- * Server handles querying/updating the remote notmuch database (and any other
--   remote resources we need).


import System.Process

import Control.Monad (void)
import System.IO     (Handle, hClose)

import qualified Data.ByteString as B

-- | an open connection
data Conn = Conn
    { sshPid :: ProcessHandle
    , sshIn  :: Handle
    , sshOut :: Handle
    }

-- | Write a bytestring to the connection
writeConn :: Conn -> B.ByteString -> IO ()
writeConn Conn{..} = B.hPut sshIn

-- | Read the specified number of bytes from the connection
readConn :: Conn -> Int -> IO B.ByteString
readConn Conn{..} count = B.hGet sshOut count

-- | @'connectSsh' args@ spawns an ssh process with the given arguments, and
-- returns a connection comprised of the process's stdin & stdout.
connectSsh :: [String] -> IO Conn
connectSsh args = do
    let cmd = (proc "ssh" args) { std_in = CreatePipe, std_out = CreatePipe }
    (Just input, Just output, Nothing, pid) <- createProcess cmd
    return $ Conn pid input output

-- | Close the connection, and wait for the process.
disconnect :: Conn -> IO ()
disconnect (Conn pid input output) = do
    hClose input
    hClose output
    void $ waitForProcess pid
