module Day11
  ( test
  , part1
  , part2
  , part1'
  , part2'
  , part1''
  , part2''
  ) where


import Lib
import Data.Map qualified as M
import Data.Map (Map)
import Data.Vector.Mutable qualified as MV
import Data.Vector.Mutable (MVector)
import Data.Vector qualified as V
import Data.Vector (Vector)
import Data.Vector.Unboxed qualified as UV
import Data.Vector.Unboxed.Mutable qualified as MUV
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Parallel.Strategies

{-
  - Find the fuel cell's rack ID, which is its X coordinate plus 10.
  - Begin with a power level of the rack ID times the Y coordinate.
  - Increase the power level by the value of the grid serial number (your puzzle input).
  - Set the power level to itself multiplied by the rack ID.
  - Keep only the hundreds digit of the power level (so 12345 becomes 3; numbers with no hundreds digit become 0).
  - Subtract 5 from the power level.

  1. Define function From Coordinate, Serial Number to Power of Cell

  2. Define 300x300 grid of power cells

  3. Iterate through all of 3x3 patch and get the maximum position.

  4. Define function generating delta positions from NxN to (N+1)x(N+1)
     and get the delta power.

  Reusing previous NxN calculation vs Reusing Current calculation

  1. If we choose option 2, we don't have to filter points from previous calcution.

     P 2 2                P 3 2
  . . . . . . .      . . . . . . .
  . . . . . . .      . . . . . . .
  . . x x x . .      . . . x x n .
  . . x x x . .  ->  . . . x x n .
  . . x x x . .      . . . x x n .
  . . . . . . .      . . . . . . .
  . . . . . . .      . . . . . . .
       |
       v
     P 2 3
  . . . . . . .
  . . . . . . .
  . . . . . . .
  . . x x x . .
  . . x x x . .
  . . n n n . .
  . . . . . . .



  p11 p21 p31 ... pn1
  p12 p22         pn2
  p13 p33 .       pn3
   .        .      .
   .           .   .
  p1n p2n     ... pnn

  If we reverse row and column of the points, the filtering can be
  accomplished just by tailing.

  Final performance is not so satisfying but, It works. (tm).

  See you next time.!
-}

type Serial = Int


data Pos = P
  { px :: Int
  , py :: Int
  } deriving stock (Show, Eq)


instance Ord Pos where
  compare (P x1 y1) (P x2 y2) =
    let cmpy = compare y1 y2
    in  if cmpy == EQ then compare x1 x2 else cmpy


toIdx :: Pos -> Int
toIdx (P x y) = assert (x >= 1 && x <= 300 && y >= 1 && y <= 300) $
  (y - 1) * 300  + x - 1


fromIdx :: Int -> Pos
fromIdx i = assert ( i >= 0 && i < 300 * 300) $
  let (y, x) = i `divMod` 300
  in  P (x + 1) (y + 1)


toIdx' :: Int -> Int  -> Int
toIdx' x y = assert (x >= 1 && x <= 300 && y >= 1 && y <= 300) $
  (y - 1) * 300  + x - 1


testIdx :: IO ()
testIdx = do
  let ps     = [ P x y | y <- [1..300], x <- [1..300] ]
      idxes  = fmap toIdx ps
      ps'    = fmap fromIdx idxes
      idxes' = fmap toIdx ps'
  assert (ps == ps') $ pure ()
  assert (idxes == idxes') $ pure ()
  assert (all (\i -> i >= 0 && i < 300 * 300) idxes) $ pure ()



pos2Power :: Serial -> Int -> Int -> Int
pos2Power serial x y =
  let rackID = x + 10
      power0 = y * rackID
      power1 = power0 + serial
      power2 = power1 * rackID
      power3 = (power2 `div` 100) `mod` 10
      power4 = power3 - 5
  in  power4


renderPowers :: [[Int]] -> String
renderPowers xss =
  foldMap (<> "\n") $ fmap renderLine xss
  where
    renderLine xs = foldMap (<> " ") $ fmap (printf "%2d") xs



testPower :: IO ()
testPower = do
  let serial  = 18
      n       = 5
      topLeft = P 32 44
      ps      = [ P (px topLeft + dx) (py topLeft + dy)
                | dy <- [0..n - 1]
                , dx <- [0..n - 1] ]
      powers  = fmap (\(P x y) -> pos2Power serial x y) ps
      output  = renderPowers $ chunksOf n powers
  putStrLn output

  let serial  = 42
      n       = 5
      topLeft = P 20 60
      ps      = [ P (px topLeft + dx) (py topLeft + dy)
                | dy <- [0..n - 1]
                , dx <- [0..n - 1] ]
      powers  = fmap (\(P x y) -> pos2Power serial x y) ps
      output  = renderPowers $ chunksOf n powers
  putStrLn output


generateGrid :: Serial -> Vector Int
generateGrid serial = V.generate (300 * 300) $ \i ->
   let (P x y) = fromIdx i
   in  pos2Power serial x y


nextPos :: Int -> [Pos] -> [Pos]
nextPos n ps  = filter pred ps
  where
    pred (P x y) =  (x <= 300 - n + 1) && (y <= 300 - n + 1)


deltaNeighborsN :: Vector Int -> Pos -> Int -> Int
deltaNeighborsN grid (P x y) n =
  let corner     = grid V.! (toIdx' (x + n - 1) (y + n - 1))
      vertical   = sum [ grid V.! (toIdx' (x + n - 1) (y + dy)) | dy <- [0 .. n - 2]]
      horizontal = sum [ grid V.! (toIdx' (x + dx) (y + n - 1)) | dx <- [0 .. n - 2]]
  in  corner + vertical + horizontal


powerN :: Vector Int -> MVector s Int -> Int -> Pos -> StateT [Pos] (ST s) Int
powerN grid prevPowers n p = do
  let deltaPower = deltaNeighborsN grid p n
  ppw <- MV.read prevPowers (toIdx p)
  pure $ ppw + deltaPower


maxPowerN
  :: Vector Int
  -> MVector s Int
  -> [(Int, Pos, Int)]
  -> Int
  -> StateT [Pos] (ST s) [(Int, Pos, Int)]
maxPowerN grid powers acc n = do
  ps' <- nextPos n <$> get
  pws <- forM ps' $ \p -> do
    currPower <- powerN grid powers n p
    MV.write powers (toIdx p) currPower
    pure (currPower, p, n)
  put ps'
  pure (pws <> acc)


totalMaxPower :: String -> Int -> (Int, Pos, Int)
totalMaxPower s n =
  let power1 = generateGrid (read s)
      ps1x1  = [ P x y | y <- [1..300] , x <- [1..300] ]
  in  runST $ flip evalStateT ps1x1 $ do
        mv      <- V.thaw power1
        entries <- foldM (maxPowerN power1 mv) [] [2.. n]
        pure (maximumOn tfst entries)

{-
    Partial Sum scheme
    Reference: https://en.wikipedia.org/wiki/Summed-area_table
-}

toIdx'' :: Int -> Int  -> Int
toIdx'' x y = assert (x >= 0 && x <= 300 && y >= 0 && y <= 300) $
  y * 300  + x


generateGrid' :: Serial -> UV.Vector Int
generateGrid' serial = runST $ do
  mv <- MUV.new (301 * 301)
  forM_ [0..300] $ \i -> do
    MUV.write mv (toIdx'' i 0) 0
    MUV.write mv (toIdx'' 0 i) 0
  forM_ [1..300] $ \y -> do
    forM_ [1..300] $ \x -> do
      let power = pos2Power serial x y
      upper  <- MUV.read mv (toIdx'' (x    ) (y - 1))
      left   <- MUV.read mv (toIdx'' (x - 1) (y    ))
      corner <- MUV.read mv (toIdx'' (x - 1) (y - 1))
      MUV.write mv (toIdx'' x y) (upper + left - corner + power)
  UV.freeze mv


powerN' :: UV.Vector Int -> Int -> Pos -> Int
powerN' grid n (P x y) =
  let tlPower = grid UV.! (toIdx'' (x - 1    ) (y - 1    ))
      trPower = grid UV.! (toIdx'' (x + n - 1) (y - 1    ))
      blPower = grid UV.! (toIdx'' (x - 1    ) (y + n - 1))
      brPower = grid UV.! (toIdx'' (x + n - 1) (y + n - 1))
  in  (tlPower - trPower - blPower + brPower)


nextPos' :: [[Pos]] -> [[Pos]]
nextPos' ps  = fmap tail $ tail ps


maxPowerN'
  :: Monad m
  => UV.Vector Int
  -> [(Int, Pos, Int)]
  -> Int
  -> StateT [[Pos]] m [(Int, Pos, Int)]
maxPowerN' grid acc n = do
  pss <- nextPos' <$> get
  let pws    = fmap (\p -> (powerN' grid n p, p, n)) $ concat pss
      maxPws = maximumOn tfst pws
  put pss
  pure (maxPws:acc)


totalMaxPower' :: String -> Int -> (Int, Pos, Int)
totalMaxPower' s n =
  let power1 = generateGrid' (read s)
      ps1x1  = reverse $
               fmap reverse $
               chunksOf 300 [ P x y | y <- [1..300] , x <- [1..300] ]
  in  flip evalState ps1x1 $ do
        entries <- parFoldM (maxPowerN' power1) [] [2.. n]
        pure (maximumOn tfst entries)



maxPowerN''
  :: UV.Vector Int
  -> (Int, [Pos])
  -> (Int, Pos, Int)
maxPowerN'' grid (n, ps) =
  let pws    = fmap (\p -> (powerN' grid n p, p, n)) ps
  in  maximumOn tfst pws


totalMaxPower'' :: String -> Int -> (Int, Pos, Int)
totalMaxPower'' s n =
  let power1  = generateGrid' (read s)
      ps1x1   = reverse $
                fmap reverse $
                chunksOf 300 [ P x y | y <- [1..300] , x <- [1..300] ]
      pss     = fmap concat $ tail $ iterate nextPos' ps1x1
      entries = parMap rpar (maxPowerN'' power1) (zip [2.. n] pss)
  in  (maximumOn tfst entries)



parFoldM :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
parFoldM f b = foldM f b . withStrategy (parList rseq)

testNextPos :: IO ()
testNextPos = do
  let ps   = [ P x y | y <- [1..300] , x <- [1..300] ]
      pss  = reverse $ fmap reverse $ chunksOf 300 ps
      ps3  = sort . nextPos 3 $ ps
      ps3' = sort $ concat $ nextPos' . nextPos' $ pss
  assert (ps3 == ps3') $ pure ()


-- Public functions --

part1 :: IO ()
part1 = interact' "input11.txt" $ \line -> do
  let (maxPower, pos, maxN) = totalMaxPower line 3
  printf "Part1: Pos: %s, Power: %d, Grid Size %d\n" (show pos) (maxPower) maxN


part2 :: IO ()
part2 = interact' "input11.txt" $ \line -> do
  let (maxPower, pos, maxN) = totalMaxPower line 300
  printf "Part2: Pos: %s, Power: %d, Grid Size %d\n" (show pos) (maxPower) maxN


part1' :: IO ()
part1' = interact' "input11.txt" $ \line -> do
  let (maxPower, pos, maxN) = totalMaxPower' line 3
  printf "Part1: Pos: %s, Power: %d, Grid Size %d\n" (show pos) (maxPower) maxN


part2' :: IO ()
part2' = interact' "input11.txt" $ \line -> do
  let (maxPower, pos, maxN) = totalMaxPower' line 300
  printf "Part2: Pos: %s, Power: %d, Grid Size %d\n" (show pos) (maxPower) maxN


part1'' :: IO ()
part1'' = interact' "input11.txt" $ \line -> do
  let (maxPower, pos, maxN) = totalMaxPower'' line 3
  printf "Part1: Pos: %s, Power: %d, Grid Size %d\n" (show pos) (maxPower) maxN


part2'' :: IO ()
part2'' = interact' "input11.txt" $ \line -> do
  let (maxPower, pos, maxN) = totalMaxPower'' line 300
  printf "Part2: Pos: %s, Power: %d, Grid Size %d\n" (show pos) (maxPower) maxN


test :: IO ()
test = do
  let power1 = generateGrid 7139
      ps1x1  = [ P x y | y <- [1..300] , x <- [1..300] ]
  let (maxPower, pos, maxN) =  runST $ do
        flip evalStateT ps1x1 $ do
          mv <- V.thaw power1
          entries <-
              foldM (maxPowerN power1 mv) [] [2.. 300]
          pure (maximumOn tfst entries)
  printf "Pos: %s, Power: %d, Grid Size %d\n" (show pos) (maxPower) maxN
  pure ()
