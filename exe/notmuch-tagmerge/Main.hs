{-# LANGUAGE OverloadedStrings #-}
module Main where

import DumpFile           (mergeMaps, parseDump, showDump)
import System.Environment (getArgs)
import System.Exit        (exitFailure)

import qualified Data.ByteString.Char8 as B8

main = do
    args <- getArgs
    case args of
        [old, left, right, result] -> nmtmerge old left right result
        _                          -> usage
  where
    usage = do
        putStrLn "usage : nmtmerge <old> <left> <right> <result>"
        exitFailure
    nmtmerge old left right result = do
        newDump <- mergeMaps
            <$> (parseDump <$> B8.readFile old)
            <*> (parseDump <$> B8.readFile left)
            <*> (parseDump <$> B8.readFile right)
        B8.writeFile result (showDump newDump)
