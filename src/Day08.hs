module Day08
  ( test
  ) where

{-
Specifically, a node consists of:

    A header, which is always exactly two numbers:
        The quantity of child nodes.
        The quantity of metadata entries.
    Zero or more child nodes (as specified in the header).
    One or more metadata entries (as specified in the header).

Each child node is itself a node that has its own header, child nodes, and metadata. For example:

<2 3>0 3  10 11 12       1 1 0 1 |99| |2| |1 1 2|
A--------------------------------------------------
  \                       \
    <0 3>|10 11 12|     <1 1>0 1 |99| |2|  1 1 2
    B-------------      C----------------
                             \
                          <0 1>|99|
                          D--------

    1. take two items. nChild, nMeta
    2. rest of the input ?? What to do?
      a) if nChild is zero then. just take nMeta items and return rest of it.
      b) if nChild is greater than zero -> repeat recursion nChild times.
-}

import Lib
import Data.Map qualified as M
import Data.Map (Map)
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State


-- We need keep track which node we are currently in
-- to record the node values.

recurse2 :: [Int] -> ([Int], Int)
recurse2 xs = -- trace (show xs) $ assert (length xs > 2) $
  let ([nChild, nMeta], rest) = splitAt 2 xs
  in  if nChild == 0 then
        let (metas, rest') = splitAt nMeta rest
        in  (rest', sum metas)
      else
        let (rest', childValues) = foldl accf (rest, []) [1..nChild]
            (metas , rest'')     = splitAt nMeta rest'
            mChildValue          = M.fromList childValues
            referencedValues     = fmap (lookupChildMeta mChildValue) metas
        in  (rest'', sum referencedValues)
  where
    lookupChildMeta m i = fromMaybe 0 $ M.lookup i m

    accf (xs, acc) i = let (xs', value) = recurse2 xs
                       in  (xs', (i, value):acc)


recurse :: [Int] -> Writer [Int] [Int]
recurse xs = do -- assert (length xs > 2) $ do
  let ([nChild, nMeta], rest) = splitAt 2 xs
  if nChild == 0 then do
    let (metas, rest') = splitAt nMeta rest
    tell metas
    pure rest'
  else do
    result <- iterateMN (nChild) recurse rest
    let rest'            = result
        (metas , rest'') = splitAt nMeta rest'
    tell metas
    pure rest''


parse :: String -> [Int]
parse s = fmap read $ splitOn " " s


iterateMN :: Monad m => Int -> (a -> m a) -> a -> m a
iterateMN n f x =
  if n == 0 then pure x else f x >>= iterateMN (n - 1) f


part1 :: IO ()
part1 = interact' "input08.txt" $ \line -> do
  let xs             = parse line
      (rest, metas)  = runWriter $ recurse xs
  printf "Sum of all meta values: %d\n" (sum metas)


part2 :: IO ()
part2 = interact' "input08.txt" $ \line -> do
  let xs              = parse line
      (rest, metaSum) = recurse2 xs
  printf "Sum of all meta values with referencing scheme: %d\n" metaSum


test :: IO ()
test = do
  let inp = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
      xs  = parse inp
  print xs
  let (rest, metas)  = runWriter $ recurse xs
  putStrLn "  ---  recurse test --- "
  print metas
  printf "Sum of all meta values: %d\n" (sum metas)

  putStrLn " --- recurse2 test --- "
  let (rest2, metaSum)  = recurse2 xs
  printf "Sum of all meta values with referecing: %d\n" (metaSum)
  mapM_ print rest2
  pure ()
