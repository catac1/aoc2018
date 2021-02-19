module Day07
  ( test
  ) where


import Lib
import Data.Map qualified as M
import Data.Map (Map)
import Data.Set qualified as S
import Data.Set (Set)


{-

    H ----> E ---> A
      \           / |
       G --> O -->  |
 P ----------/ \    |
           /     -Y-|
  M ------ \        /
            \- L ---
              /
   X -------->

It's was not dfs. :<


- H M P X
H - E G M P X
H E - G M P X
H E G - M P X
H E G M - P X
H E G M P - O X
H E G M P O - Y X
H E G M P O Y - X
H E G M P O Y X - L A
H E G M P O Y X L - A
H E G M P O Y X L A -



 ---- node Map ---
('A',"BD")
('B',"E")
('C',"AF")
('D',"E")
('F',"E")

 ---- previous node Map ---
('A',"C")
('B',"A")
('D',"A")
('E',"BDF")
('F',"C")

              C A B E B A D E D A C F E F C
 order record ^ ^ ^       ^         ^ ^
 backtrace          * * *   * * * *     * *
              C A B B A D D A F E F
  CABDFE
1. make adjcent node map

2. Traverse nodes and record the order.

3  When all the records of previous nodes are set
   record the order of current node

-}

parse :: [String] -> [(Char, Char)]
parse ss = fmap parseEntry ss
  where
    parseEntry s =
      let [src, dst] = splitOn " must be finished before step " s
      in  (last src, head dst)


nextNodes :: [(Char, Char)] -> Map Char [Char]
nextNodes entries = foldl accf M.empty $ reverse $ sortOn snd entries
  where
    accf :: Map Char [Char] -> (Char, Char) -> Map Char [Char]
    accf m (src, dst) = M.alter (\mc -> Just $ dst:(fromMaybe [] mc)) src m


prevNodes :: [(Char, Char)] -> Map Char [Char]
prevNodes entries = foldl accf M.empty $ reverse $ sortOn snd $ fmap swap entries
  where
    accf :: Map Char [Char] -> (Char, Char) -> Map Char [Char]
    accf m (src, dst) = M.alter (\mc -> Just $ dst:(fromMaybe [] mc)) src m


bfs :: Map Char [Char] -> Set Char -> [Char]
bfs mPrev steps = if S.null steps then [] else
  let restStep         = S.delete curStep steps
      curStep          = S.elemAt 0 steps
      updatedPrevMap   = M.map (delete curStep) mPrev
      (mAvail, mPrev') = M.partition null updatedPrevMap
      steps'           = S.union restStep (S.fromList $ M.keys mAvail)
  in  -- trace (show steps')
      curStep : bfs mPrev' steps'


-- Loop step should be done per second.
-- Decrease remained time of a step to complete.
-- when it's done, we can add new available steps to othe queue.
-- We need to express currently processed step list.
--
{-
      1. take available steps put into in progress queue
      2. decrease remaining time of the steps in the queue
      3. if there's steps has been done , put them in the available steps.
-}

toWIPStep :: Char -> (Char, Int)
toWIPStep c = (c, ord c - ord 'A' + 61)
-- toWIPStep c = (c, ord c - ord 'A' + 1)


decreaseStep :: (Char, Int) -> (Char, Int)
decreaseStep (c, i) = (c, i - 1)


isStepDone :: (Char, Int) -> Bool
isStepDone (_, i) = i == 0

-- Yay!

bfsWithHelp :: Int -> Map Char [Char] -> [(Char, Int)] -> Set Char -> [(Char, Int)]
bfsWithHelp timestamp mPrev wipSteps steps =
  if null wipSteps && S.null steps then [] else
  -- let availAbleWorkers           = 2 - length wipSteps -- can take up to 5
  let availAbleWorkers           = 5 - length wipSteps -- can take up to 5
      (curStep, restStep)        = S.splitAt availAbleWorkers steps
      (completedStep, wipSteps') = partition isStepDone
                                   $ fmap decreaseStep
                                   $ wipSteps <> (fmap toWIPStep $ S.toList curStep)
      completedStepIds           = fmap fst completedStep
      updatedPrevMap             = M.map (filter (\s -> not $ s `elem` completedStepIds)) mPrev
      (mAvail, mPrev')           = M.partition null updatedPrevMap
      steps'                     = S.union restStep (S.fromList $ M.keys $ mAvail)
      timestamp'                 = timestamp + 1
  in  -- trace (show steps')
      if not . null $ completedStep then
        (fmap (,timestamp) completedStepIds) <>
        bfsWithHelp timestamp' mPrev' wipSteps' steps'
      else
        bfsWithHelp timestamp' mPrev' wipSteps' steps'


begNodes :: Map Char [Char] -> [Char]
begNodes m =
  let srcs = S.fromList $ M.keys m
      dsts = S.fromList $ concat $ M.elems m
  in  S.toList $ srcs `S.difference` dsts


part1 :: IO ()
part1 = interactS "input07.txt" $ \lines -> do
  let nextMap  = nextNodes $ parse lines
      prevMap  = prevNodes $ parse lines
      topNodes = begNodes nextMap
      ans      = bfs prevMap (S.fromList topNodes)
  printf "Part1: Traversing order is %s\n" ans


part2 :: IO ()
part2 = interactS "input07.txt" $ \lines -> do
  let nextMap    = nextNodes $ parse lines
      prevMap    = prevNodes $ parse lines
      topNodes   = begNodes nextMap
  let jobRecords = bfsWithHelp 0 prevMap [] (S.fromList topNodes)
  printf "Part2: Total time spent to do all the steps: %d\n" (1 + (snd . last $ jobRecords))


test :: IO ()
test = do
  let inps =
        [ "Step C must be finished before step A can begin."
        , "Step C must be finished before step F can begin."
        , "Step A must be finished before step B can begin."
        , "Step A must be finished before step D can begin."
        , "Step B must be finished before step E can begin."
        , "Step D must be finished before step E can begin."
        , "Step F must be finished before step E can begin." ]
  let nextMap  = nextNodes $ parse inps
      prevMap  = prevNodes $ parse inps
      topNodes = begNodes nextMap
  putStrLn " ---- next node Map --- "
  mapM_ print (M.toList nextMap)

  putStrLn " ---- previous node Map --- "
  mapM_ print (M.toList prevMap)

  putStrLn " --- begin node --- "
  mapM_ print topNodes

  putStrLn " ---- bfs --- "
  print $ bfs prevMap (S.fromList topNodes)

  let jobRecords = bfsWithHelp 0 prevMap [] (S.fromList topNodes)
  putStrLn " --- bfsWithHelp --- "
  mapM_ print (jobRecords)
  printf "Total time spent to do all the steps: %d\n" (1 + (snd . last $ jobRecords))
  pure ()
