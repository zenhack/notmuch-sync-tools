{-# LANGUAGE OverloadedStrings #-}
import Data.Maybe         (catMaybes, fromMaybe)
import System.Environment (getArgs)

import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Merge.Strict as M
import qualified Data.Map.Strict       as M
import qualified Data.Set              as S

mergeTags ancestor left right = added `S.union` inBoth
  where
    added = (left `S.union` right) `S.difference` ancestor
    inBoth = left `S.intersection` right

mergeDumps ancestor = M.merge
    (M.mapMissing $ \k l -> mergeVal k l S.empty)
    (M.mapMissing $ \k r -> mergeVal k S.empty r)
    (M.zipWithMatched mergeVal)
  where
    mergeVal k = mergeTags (fromMaybe S.empty $ M.lookup k ancestor)

parse = B.lines
    .> map (B.words .> reverse .> parseLine)
    .> catMaybes
    .> M.fromList
  where
    parseLine (name:"--":tags) = Just $ (name, S.fromList tags)
    parseLine _                = Nothing

(.>) = flip (.)

showDump = M.toList .> map showLine .> B.unlines
  where
    showLine (name, tags) = B.unwords $ reverse $ name:"--":S.toList tags

main = do
    args <- getArgs
    case args of
        [ancestor, left, right, result] -> main' ancestor left right result
        _                               -> usage
  where
    usage = putStrLn "usage: nmtmerge <ancestor> <left> <right> <result>"
    main' ancestor left right result =
        B.writeFile result =<< showDump <$> (mergeDumps
            <$> (parse <$> B.readFile ancestor)
            <*> (parse <$> B.readFile left)
            <*> (parse <$> B.readFile right))
