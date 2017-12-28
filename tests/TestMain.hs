{-# LANGUAGE RecordWildCards #-}
module Main where

import DumpFile
import Query

import Control.Monad (when)

import qualified Data.HashMap.Strict as M

main = do
    DBInfo{..} <- getDBInfo
    PartialDump pDump <- getDumpSince lastMod
    when (M.null pDump) $ do
            error $ "partial dump from 'now' returned no results!\n" ++
                "At least *one* message should have been modified."
