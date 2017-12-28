{-# LANGUAGE OverloadedStrings #-}
module DumpFile where

import Data.Function (on, (&))
import Data.Maybe    (catMaybes)

import qualified Data.ByteString.Char8 as B8
-- Experimentally, Set + HashMap performs better than HashSet + HashMap or Set +
-- Map, presumably because the sets are very small and the maps are big.
import qualified Data.HashMap.Strict as M
import qualified Data.Set            as S

type Dump = M.HashMap B8.ByteString (S.Set B8.ByteString)

type TagSet = S.Set B8.ByteString

-- | A "partial" dump. This is the same thing as a dump, but does not include all
-- messages; just a subset that have been modified "recently."
newtype PartialDump = PartialDump Dump

-- | Like @'mergeMaps'@ except that @left@ and @right@ are partial dumps. They
-- must both include all messages modified since @old@.
mergePartial :: Dump -> PartialDump -> PartialDump -> Dump
mergePartial old (PartialDump leftPart) (PartialDump rightPart) =
    mergeMaps old (M.union leftPart old) (M.union rightPart old)

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

-- 'parseDump' parses its argument, which must be in the format emitted by
-- @notmuch-dump(1)@, into a 'Dump'. Note that the parser simply ignores lines
-- that don't fit the format it expects, so this won't ever actually fail.
--
-- TODO: we probably can (and should) make the parser a little more
-- restrictive; I(zenhack) think all non-coforming lines start with '#'.
parseDump :: B8.ByteString -> Dump
parseDump input =
    B8.lines input &
    map parseSet &
    catMaybes &
    M.fromList
  where
    parseSet line = case reverse (B8.words line) of
        (name:"--":tags) -> Just (name, S.fromList tags)
        _                -> Nothing

-- 'showDump' outputs its argument formatted as emitted by @notmuch-dump(1)@
-- (and expected by @notmuch-restore@).
showDump :: Dump -> B8.ByteString
showDump dump =
    M.toList dump &
    map showLine &
    B8.unlines
  where
    showLine (name, tags) =
        name:"--":S.toList tags &
        reverse &
        B8.unwords
