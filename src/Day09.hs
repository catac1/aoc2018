module Day09
  ( test
  , part1
  , part2
  ) where


import Lib
import Control.Monad.Trans.State
import Data.Vector.Mutable qualified as MV
import Data.Vector.Mutable (MVector)
import Data.Vector qualified as V
import Data.Vector (Vector)


insertMarble
  :: Int                  -- ^ Number of total players.
  -> MVector s Int        -- ^ Player scores.
  -> MVector s (Int, Int) -- ^ Bidirectional linked list of the marbles.
  -> Int                  -- ^ Current marble position.
  -> Int                  -- ^ The worth of the new marble.
  -> ST s Int             -- ^ Returns next marble position.
insertMarble nPlayer mvScore mvXS marbleIdx worth =
  if worth `mod` 23 == 0 then do
    (removedMarble, marbleIdx') <- removePrev mvXS marbleIdx 7
    let player = worth `mod` nPlayer + 1
        score  = worth + removedMarble
    MV.modify mvScore (+ score) player
    pure marbleIdx'
  else do
    marbleIdx' <- insertNext mvXS marbleIdx 2 worth
    pure marbleIdx'


insertNext :: MVector s (Int, Int) -> Int -> Int -> Int -> ST s Int
insertNext mv curr0 offset0 dest = go curr0 offset0
  where
    go curr 0 = do
      (currPrev, currNext)  <- MV.read mv curr
      (prevPrev, _prevNext) <- MV.read mv currPrev
      MV.write mv currPrev (prevPrev, dest    )
      MV.write mv curr     (dest    , currNext)
      MV.write mv dest     (currPrev, curr    )
      pure dest
    go curr offset = do
      (_, next) <- MV.read mv curr
      go next (offset - 1)


showLink :: Int -> MVector s (Int, Int) -> ST s ([(Int, Int, Int)])
showLink size0 mv = go size0 0
  where
    go 0    curr = pure []
    go size curr = do
      (p, n) <- MV.read mv curr
      xs     <- go (size - 1) n
      pure ((p, curr, n):xs)


removePrev :: MVector s (Int, Int) -> Int -> Int -> ST s (Int, Int)
removePrev mv curr0 offset0 = go curr0 offset0
  where
    go curr 0 = do
      (currPrev, currNext)  <- MV.read mv curr
      (prevPrev, _prevNext) <- MV.read mv currPrev
      (_nextPrev, nextNext) <- MV.read mv currNext
      MV.write mv currPrev (prevPrev, currNext)
      MV.write mv currNext (currPrev, nextNext)
      MV.write mv curr     (-1      , -1      )
      pure (curr, currNext)
    go curr offset = do -- trace (show curr) $ do
      (prev, _) <- MV.read mv curr
      go prev (offset - 1)


parse :: String -> (Int, Int)
parse s =
  let tokens    = words s
      nPlayer   = read $ tokens !! 0
      lastWorth = read $ (tokens !! (length tokens - 2))
  in  (lastWorth, nPlayer)


setupScore :: Int -> ST s (MVector s Int)
setupScore nPlayer = do
  mv <- MV.new (nPlayer + 1)
  forM_ [0..nPlayer] $ \i ->
    MV.write mv i 0
  pure mv


setupLink :: Int -> ST s (MVector s (Int, Int))
setupLink lastWorth = do
  mv <- MV.new (lastWorth + 1)
  forM_ [0..lastWorth] $ \i ->
    MV.write mv i (-1, -1)
  -- `InsertNext` on a linked list with a single item won't work,
  -- because next and previous item points to itself.
  -- So we prepare two items in advance.
  MV.write mv 0 (1, 1)
  MV.write mv 1 (0, 0)
  pure mv


scoring :: Int -> Int -> Int
scoring lastWorth nPlayer =
  let (scores, _links) = runST $ do
        mvScore <- setupScore nPlayer
        mvLink  <- setupLink lastWorth
        let startWorth     = 2
            startMarbleIdx = 1
        _nMarble <-
              foldM
              (insertMarble nPlayer mvScore mvLink)
              startMarbleIdx
              [startWorth..lastWorth]
        vScore <- V.freeze mvScore
        vLink  <- V.freeze mvLink
        pure (V.toList vScore, V.toList vLink)
  in  maximum scores


part1 :: IO ()
part1 = interact' "input09.txt" $ \line -> do
  let (lastWorth, nPlayer) = parse line
  let ans = scoring lastWorth nPlayer
  printf "Part1: Maximum Score %2d\n" ans
  pure ()


part2 :: IO ()
part2 = interact' "input09.txt" $ \line -> do
  let (lastWorth, nPlayer) = parse line
  let ans = scoring (lastWorth * 100) nPlayer
  printf "Part2: Maximum Score %2d\n" ans
  pure ()


test :: IO ()
test = do
  let inp =
        [ "10 players; last marble is worth 1618 points"
        , "13 players; last marble is worth 7999 points"
        , "17 players; last marble is worth 1104 points"
        , "21 players; last marble is worth 6111 points"
        , "30 players; last marble is worth 5807 points" ]
  mapM_ print $ fmap (uncurry scoring . parse) inp
  pure ()
