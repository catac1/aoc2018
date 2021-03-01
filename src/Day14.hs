module Day14
  ( test
  , part1
  , part2
  ) where


{-
  I think 3.65 secs is not bad performance for this machine.
  My CPU is haswell 4690 non-K version.

  Anyway, That's it for today. See you next time.
-}


import Lib


import Data.Vector.Mutable qualified as MV
import Data.Vector.Mutable (MVector)
import Data.Vector qualified as V
import Data.Vector (Vector)



createRecipe :: Int -> ST s (MVector s Int)
createRecipe n0 = do
  mv0 <- MV.new n0
  forM_ [0..n0-1] $ \i -> MV.write mv0 i (-1)
  MV.write mv0 0 3
  MV.write mv0 1 7
  go mv0 2 0 1
  where
  go :: MVector s Int -> Int -> Int -> Int -> ST s (MVector s Int)
  go mv i first second
    | i == n0 = pure mv
    | otherwise = do
      score1 <- MV.read mv first
      score2 <- MV.read mv second
      let (lRecipe, rRecipe) = (score1 + score2) `divMod` 10
      if lRecipe == 0 then do
        MV.write mv i rRecipe
        let first'  = (first  + score1 + 1) `mod` (i + 1)
            second' = (second + score2 + 1) `mod` (i + 1)
        go mv (i + 1) first' second'
      else do
        MV.write mv i lRecipe
        if i < n0 - 1 then do
          MV.write mv (i + 1) rRecipe
          let first'  = (first  + score1 + 1) `mod` (i + 2)
              second' = (second + score2 + 1) `mod` (i + 2)
          go mv (i + 2) first' second'
        else
          pure mv


findRecipe :: [Int] -> ST s Int
findRecipe xs0 = do
  mv0 <- MV.new capacity -- How much space do we need? -> We don't know.
  MV.write mv0 0 3
  MV.write mv0 1 7
  go mv0 xs0 capacity 2 0 1
  where
    capacity = 40000000
    inpLen = length xs0
    go :: MVector s Int -> [Int] -> Int -> Int -> Int -> Int -> ST s Int
    go mv []     vecLen i _     _      = pure (i - inpLen)
    go mv (x:xs) vecLen i first second = do
      -- Vector Grow function has horrible performance when it's size is
      -- too granular or chunky.
      mv' <- if i + 1 < vecLen then pure mv else MV.grow mv capacity
      let vecLen' = if i < vecLen then vecLen else vecLen + capacity

      score1 <- MV.read mv' first
      score2 <- MV.read mv' second
      let (lRecipe, rRecipe) = (score1 + score2) `divMod` 10
      if lRecipe == 0 then do
        -- trace ("writing -> " ++ show i) $ do
        MV.write mv' i rRecipe
        let first'  = (first  + score1 + 1) `mod` (i + 1)
            second' = (second + score2 + 1) `mod` (i + 1)
            xs'     = if x == rRecipe then xs else xs0
        -- trace (show rRecipe ++ " -> " ++ show xs') $
        go mv' xs' vecLen' (i + 1) first' second'
      else do
        -- trace ("writing -> " ++ show i ++ ", " ++ show (i + 1)) $ do
        MV.write mv' (i    ) lRecipe
        MV.write mv' (i + 1) rRecipe
        let first'  = (first  + score1 + 1) `mod` (i + 2)
            second' = (second + score2 + 1) `mod` (i + 2)
            xs'     = if x == lRecipe then xs else xs0
            xs''    = if | null xs'            -> []
                         | head xs' == rRecipe -> tail xs'
                         | head xs' /= rRecipe && head xs0 == rRecipe
                                               -> tail xs0
                         | otherwise           -> xs0
        -- trace (show lRecipe ++ "," ++ show rRecipe ++ " -> " ++ show xs' ++ " - " ++ show xs'') $ do
        let i' = if null xs' then i + 1 else i + 2
        go mv' xs'' vecLen' i' first' second'



nineScores :: Int -> [Int]
nineScores n = runST $ do
  mv <- createRecipe (n + 10)
  xs <- forM [n .. n + 9] (MV.read mv)
  pure xs


findScores :: String -> Int
findScores s =
  let digits = fmap (read .pure) s
      afterN = runST $ findRecipe digits
  in  afterN


part1 :: IO ()
part1 = interact' "input14.txt" $ \line -> do
  let inp = read line
      ans = foldMap show $ nineScores inp
  printf "Part1: the score is %s\n" ans


part2 :: IO ()
part2 = interact' "input14.txt" $ \line -> do
  let ans = findScores line
  printf "Part2: %s first appears after %d\n" line ans


testCreatRecipe :: Int -> IO ()
testCreatRecipe n = do
  let recipes = runST $ do
                  mv <- createRecipe n
                  v <- V.freeze mv
                  pure (V.toList v)
  print recipes


test :: IO ()
test = do
  let inps = [9, 5, 18, 2018]
  mapM_ putStrLn $ fmap (foldMap show . nineScores) inps
  pure ()


testBackward :: IO ()
testBackward = do
  let inps =
        [ (9   , "51589")
        , (5   , "01245")
        , (18  , "92510")
        , (2018, "59414")
        , (2   , "101"  ) ]
  mapM_ print $ fmap (findScores . snd) inps
  pure ()


testgrow :: IO ()
testgrow = do
  let xs = runST $ do
             mv <- MV.new 10
             forM_ [0..4] $ \i -> MV.write mv i 1
             forM_ [5..9] $ \i -> MV.write mv i 2
             mv' <- MV.grow mv 10
             forM_ [10..19] $ \i -> MV.write mv' i 3
             v <- V.freeze mv'
             pure (V.toList v)
  print xs
