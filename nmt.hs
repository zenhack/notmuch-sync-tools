{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Function      (on, (&))
import Data.Maybe         (catMaybes)
import System.Environment (getArgs)
import System.Exit        (exitFailure)

import qualified Data.ByteString.Char8 as B8
-- Experimentally, Set + HashMap performs better than HashSet + HashMap or Set +
-- Map, presumably because the sets are very small and the maps are big.
import qualified Data.HashMap.Strict as M
import qualified Data.Set            as S

type Dump = M.HashMap B8.ByteString (S.Set B8.ByteString)

type TagSet = S.Set B8.ByteString

-- | @'mergeSets' old left right@ performs a 3-way merge of the sets @left@ and
-- @right@, using common ancestor @old@
mergeSets :: TagSet -> TagSet -> TagSet -> TagSet
mergeSets old left right = inBoth `S.union` added
  where
    inBoth = left `S.intersection` right
    added = (left `S.union` right) `S.difference` old

-- | @'mergeMaps' old left right@ performs an element-wise 3-way merge
-- of the maps @left@ and @right@, using the common ancestor @old@.
-- Absent keys are treated as having the value 'S.empty'
mergeMaps :: Dump -> Dump -> Dump -> Dump
mergeMaps old left right =
    S.toList allKeys &
    map (\key -> (key, mergeVals key)) &
    filter (not . S.null . snd) &
    M.fromList
  where
    allKeys = S.unions [S.fromList (M.keys m) | m <- [old, left, right]]
    -- | Merge the values corresponding to @key@ in each of the maps, using
    -- 'mergeSets'.
    mergeVals key = mergeSets (getVal old) (getVal left) (getVal right)
      where
        getVal = M.lookupDefault S.empty key

parseDump input =
    B8.lines input &
    map parseSet &
    catMaybes &
    M.fromList
  where
    parseSet line = case reverse (B8.words line) of
        (name:"--":tags) -> Just (name, S.fromList tags)
        _                -> Nothing

showDump dump =
    M.toList dump &
    map showLine &
    B8.unlines
  where
    showLine (name, tags) =
        name:"--":S.toList tags &
        reverse &
        B8.unwords

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
