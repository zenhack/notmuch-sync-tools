{-# LANGUAGE RecordWildCards #-}
-- | Tools for querying the local notmuch database
module Query where

import DumpFile (PartialDump(..), parseDump)

import System.Process (readProcess)
import Text.Read      (readMaybe)

import qualified Data.ByteString.Char8 as B8

data DBInfo = DBInfo
    { lastMod :: Int
    , uuid    :: B8.ByteString
    }
    deriving(Show, Read, Eq)

getDBInfo :: IO DBInfo
getDBInfo = do
    output <- readProcess "notmuch" ["count", "--lastmod"] ""
    case words output of
        [_, uuidStr, lastModStr] -> case readMaybe lastModStr of
            Nothing      -> error "TODO: raise a proper exception"
            Just lastMod -> return DBInfo
                { lastMod = lastMod
                , uuid = B8.pack uuidStr
                }

getDumpSince :: Int -> IO PartialDump
getDumpSince lastMod = do
    output <- readProcess "notmuch" ["dump", "--", "--lastmod:" ++ show lastMod] ""
    return $ PartialDump $ parseDump $ B8.pack output
