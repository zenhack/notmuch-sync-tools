{-# LANGUAGE OverloadedStrings #-}
import Data.Maybe         (catMaybes, fromMaybe)
import System.Environment (getArgs)

import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict   as M
import qualified Data.Set              as S

mergeDumps ancestor left right =
    M.unionWith S.union added inBoth
  where
    added = M.unionWith S.difference
        (M.unionWith S.union left right)
        ancestor
    inBoth = M.intersectionWith S.intersection left right

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
