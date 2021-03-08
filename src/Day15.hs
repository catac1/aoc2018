module Day15
  ( test
  , part1
  , part2
  ) where


import Lib
import Data.Vector qualified as V
import Data.Vector (Vector)


{-
  1. Using Vector for storing the Goblin and Elf locations and HP states.

  2. Vector uses Int for indexing. We define function from (x,y) coordinates
     from / to Vector index.

  3. parse input and produce Vector Cell

  4. We need to calacuate distance from a certain point in the cells.
     Simple flood fill algorithm will work.

  5. Finding the move target.
     a) search candidate positions. floodfill src to other positions.
     b) floodfill backwards from selected position to src

  6. Actual move
     Select least distance adjcent cell from movement target.

  7. Simulate movments all of the units in the cells
     a) Get the all of the unit indexes.
     b) do the move for every units.

  8. Attack!!
     a) Select fewest hit points enemy if there's any
     b) Reduce it's hp by 3

  9. Calculate the outcome
     a) sum of all the hp of the units
     b) full round * sum of hp
-}


data Cell
  = Goblin   Int -- ^ Int is for HP
  | Elf      Int -- ^ Int is for HP
  | Distance Int -- ^ Int is for distance from certain points on the map
  | Wall
  | Empty
  | Custom   Char -- ^ For Debugging purposes



isElf :: Cell -> Bool
isElf (Elf _) = True
isElf _       = False


isGoblin :: Cell -> Bool
isGoblin (Goblin _) = True
isGoblin _          = False


showCell :: Cell -> Char
showCell cell = case cell of
  (Goblin   _) -> 'G'
  (Elf      _) -> 'E'
  (Distance d) -> (chr $ d `mod` 10 + ord '0')-- We just show only least significant digit.
  Wall         -> '#'
  Empty        -> '.'
  (Custom c)   -> c


toCell :: Char -> Cell
toCell c = case c of
  'G' -> Goblin 200
  'E' -> Elf    200
  '.' -> Empty
  '#' -> Wall
  _   -> Empty  -- it's a little itch but for now just ignore the edge cases.


toIdx :: Int -> (Int, Int) -> Int
toIdx w (x, y) = y * w + x


fromIdx :: Int -> Int -> (Int, Int)
fromIdx w i = let (y, x) = i `divMod` w in (x, y)


-- Adjcent positions from a index
-- But we need to filter out invalid index such as (-1,0)
adjIdx :: Int -> Int -> Int -> [Int]
adjIdx w h i = let (x, y) = fromIdx w i in
  fmap (toIdx w) $
  filter isValid $
  [(x, y - 1), (x - 1, y), (x + 1, y), (x, y + 1)]
  where
    isValid (x, y) = x >= 0 && x < w && y >= 0 && y < h


showIdx :: Int -> Int -> String
showIdx w i = show $  fromIdx w i


testIdx :: IO ()
testIdx = do
  let ps = [(4,2), (0,0)]
      w = 7
      h = 5
      results = fmap (\p -> (p, adjIdx w h (toIdx w p))) ps
  forM_ results $ \(p, neighbors) -> do
    printf "Neighbors of %s\n" (show p)
    mapM_ (print . fromIdx w) neighbors
    print " --- "


parse :: [String] -> Vector Cell
parse ss = V.fromList $ fmap toCell $ concat ss


showCellsWithPatch :: Int -> Vector Cell -> [(Int, Cell)] -> String
showCellsWithPatch w v patch =  showCells w (v V.// patch)


showCells :: Int -> Vector Cell -> String
showCells w v =
  intercalate "\n" $ chunksOf w $ fmap showCell $  V.toList v


showCellsWithHp :: Int -> Vector Cell -> String
showCellsWithHp w cells =
  let statOut = fmap toHpStat $
                chunksOf w $
                V.toList $
                V.imap (\i cell -> (i, cell)) cells
      cellOut = chunksOf w $
                fmap showCell $
                V.toList cells
  in  intercalate "\n" $ zipWith (\co so -> co <> "   " <> so) cellOut statOut
  where
    toHpStat :: [(Int, Cell)] -> String
    toHpStat tups =
      let units = filter (\(_, cell) -> isGoblin cell || isElf cell) tups
      in  if null units then
            ""
          else
            intercalate ", " $ fmap (showHp . snd)  units

    showHp :: Cell -> String
    showHp cell = case cell of
      (Goblin hp) -> printf "G(%d)" hp
      (Elf    hp) -> printf "E(%d)" hp
      _           -> ""


floodFill :: Int -> Int -> Int -> Vector Cell -> Vector Cell
floodFill w h src cells0 =
  go [(src, 0)] (cells0 V.// [(src, Distance 0)]) -- Starting Cell distance is zero.
  where
    toReachable :: Vector Cell -> Int -> Int -> Maybe (Int, Int)
    toReachable cells d1 i = case cells V.! i of
      (Distance d2) -> if (d1 + 1) < d2 then Just (i, d1 + 1) else Nothing
      Empty         -> Just (i, d1 + 1)
      _             -> Nothing

    go []          cells = cells
    go ((q, d):qs) cells =
      let adjs   = catMaybes $ fmap (toReachable cells d) $ adjIdx w h q
          cells' = cells V.// (fmap (\(i, d) -> (i, Distance d)) adjs)
          qs'    = qs <> adjs
      in  go qs' cells'


searchMoveTarget :: Int -> Int -> Int -> Vector Cell -> Int
searchMoveTarget w h src cells0 =
  -- move targets exists only in adjcent positions to the enemies
  let distCells  = floodFill w h src cells0
      es         = getEnemyIndexes src cells0 :: [Int]
      esAdjs     = concatMap (adjIdx w h) es
      -- What about duplicates?
      -- Nah, doesn't matter because we will filter out those by selecting mimimum distance position.
      candidates = V.toList $
                   V.map    (\(i, cell) -> (i, dist2int cell)) $
                   V.filter (\(i, cell) -> isReachable i esAdjs cell) $
                   V.imap   (\i cell    -> (i, cell)) distCells
      -- Why just minimum works even though we need prioritize reading order when selecting
      -- tie distances?  We parsed input into Vector with reading order :).
  in  if null candidates then -- trace (show src <> " will not move") $
        src
      -- trace ("\n" <> showCells w distCells  <> "\n" <>
      --        show (fmap (fromIdx w) es) <> "\n" <>
      --        show (fmap (fromIdx w) esAdjs) <> "\n" <>
      --        intercalate ", " (fmap (\(i, d) -> show (fromIdx w i)  <> " -> " <> show d) candidates)) $
      else
        let movTgt = minimum $ fmap fst $ minimumsOn snd candidates
        in  -- trace ("Move target of " <> showIdx w src <> " is " <> showIdx w movTgt) $
            movTgt
  where
    -- How do we identfy enemy???
    -- get the current src cell type?? and filter out oppisite type cells.
    isReachable :: Int -> [Int] -> Cell -> Bool
    isReachable i esAdjs distCell = case distCell of
       (Distance _) -> i `elem` esAdjs
       Empty        -> False -- Do we really hit this even though after floodfill?
                             -- The answer is YES. because there will be empty cell
                             -- which cannot be reached due to blocking from other units.
       _            -> False


getEnemyIndexes :: Int -> Vector Cell -> [Int]
getEnemyIndexes src cells = let srcCell = cells V.! src in
  case srcCell of
    (Goblin _) -> cells `filterBy` isElf
    (Elf    _) -> cells `filterBy` isGoblin
    _          -> []

-- it's unfortune that this function can't be polymorphic over containers
-- due to imap usage.
filterBy :: Vector Cell -> (Cell -> Bool) -> [Int]
filterBy cells pred =
  fmap fst $ V.toList $
  V.filter (\(i, cell) -> pred cell) $
  V.imap (\i cell -> (i, cell)) $ cells


dist2int :: Cell -> Int
dist2int (Distance d) = d
dist2int _            = maxBound


move :: Int -> Int -> Int -> Int -> Vector Cell -> (Int, Vector Cell)
move w h src tgt cells0 =
  if src == tgt then (src, cells0) else
  let distCells     = floodFill w h tgt cells0
      srcCell       = cells0 V.! src
      adjs          = fmap (\i -> (i, dist2int $ distCells V.! i)) (adjIdx w h src)
      movCandidates = minimumsOn snd adjs
                  -- hm.. What if there no distance cells on the adjcent?
                  -- NOTE: Common haskell gotcha. minimum is a partial function
                  --       which means it throws error when the input is empty list.
  in  if null movCandidates then
        (src, cells0)
      else
        let src' = minimum $ fmap fst $ movCandidates
        in  (src', cells0 V.// [(src', srcCell), (src, Empty)])

-- Attack seems working
attack :: Int -> Int -> Int -> Int -> Vector Cell -> Vector Cell
attack w h ap src cells0 =
  let adjs = getAdjEnemies w h src cells0 in
  if  null adjs then
    cells0
  else
    let (tgt, tgtCell) = selectMinHpEnemy adjs
        tgtCell'       = reduceEnemyHp tgtCell
        -- Handle the case where enemy dies.
    in  -- trace (showIdx w src <> " -> " <> showIdx w tgt) $
        if isEnemyDead tgtCell' then
          cells0 V.// [(tgt, Empty)]
        else
          cells0 V.// [(tgt, tgtCell')]
  where
    -- We don't have to worry about blowing up minimumOn
    -- because the not null is garanteed from previous
    -- code path.
    selectMinHpEnemy :: [(Int, Cell)] -> (Int, Cell)
    selectMinHpEnemy idxNCells = minimumOn (unit2hp . snd) idxNCells

    isEnemyDead (Goblin hp) = hp <= 0
    isEnemyDead (Elf    hp) = hp <= 0
    isEnemyDead _           = False

    reduceEnemyHp (Goblin hp) = Goblin (hp - ap)
    reduceEnemyHp (Elf    hp) = Elf    (hp - 3)
    reduceEnemyHp cell        = cell


unit2hp :: Cell -> Int
unit2hp (Goblin  hp) = hp
unit2hp (Elf     hp) = hp
unit2hp _            = 0


getAdjEnemies :: Int -> Int -> Int -> Vector Cell -> [(Int, Cell)]
getAdjEnemies w h src cells =
  let srcCell  = cells V.! src
      adjCells = fmap (\i -> (i, cells V.! i)) (adjIdx w h src)
  in  case srcCell of
        (Goblin _) -> filter (isElf . snd) adjCells
        (Elf    _) -> filter (isGoblin . snd) adjCells
        _          -> []


data Round = Initial | Full | Incomplete deriving stock (Eq, Show)


--- How we can identify full round?
simulateMove :: Int -> Int -> Int -> (Round, Vector Cell) -> (Round, Vector Cell)
simulateMove w h ap (_, cells0) =
  let units  = cells0 `filterBy` (\cell -> isGoblin cell || isElf cell)
  in  go units cells0
  where
    -- Currently attack is one step behind.
    -- If either goblins or elfs all dead before all units are processed, it's Incomplete round
    go []         cells = (Full, cells)
    go (src:srcs) cells =
      let goblins = V.filter (isGoblin) cells
          elves   = V.filter (isElf) cells
      in  if null goblins || null elves then
            (Incomplete, cells)
          else
            let movTgt  = searchMoveTarget w h src cells
                -- We need attack from new moved location not old one.
                (src', cells')  = move w h src movTgt cells
                cells''         = attack w h ap src' cells'
            in  go srcs cells''


calculateOutcome :: [(Round, Vector Cell)] -> (Int, Int, Int)
calculateOutcome cellss =
  let fullRounds = length (fullNIncomplete cellss) - 1
      sumOfhp    = hpSum $ snd $ last $ cellss
  in  (fullRounds, sumOfhp, fullRounds * sumOfhp)


hpSum :: Vector Cell -> Int
hpSum cells =
  sum $
  fmap (\cell -> unit2hp cell) $
  V.toList $
  V.filter (\cell -> isGoblin cell || isElf cell) cells


fullNIncomplete :: [(Round, Vector Cell)] -> [(Round, Vector Cell)]
fullNIncomplete cellss =
  takeUntil ((== Incomplete) . fst) $
  dropWhile ((== Initial) . fst) cellss


-- This is inclusive take
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil pred xs = go xs
  where
    go []     = []
    go (x:xs) = if pred x then [x] else x : go xs


part1 :: IO ()
part1 = interactS "input15.txt" $ \lines -> do
  let cells      = parse lines
      w          = length $ lines !! 0
      h          = length lines
      sims       = iterate (simulateMove w h 3) (Initial, cells)
      result     = fullNIncomplete sims
      indexes    = [0..] :: [Int]
      out        = showCellsWithHp w (snd . last $ result)
      (fullRounds, sumOfhp, outcome) = calculateOutcome result

  putStrLn out
  printf "Part1: Full Rounds(%d)  Hp sum(%d) -> %d\n" fullRounds sumOfhp outcome
  putStrLn "\n"


repeatUntil :: (a -> Bool) -> (a -> a) -> a -> (Int, a)
repeatUntil pred f a0 = go 0 a0
  where
    go n a = if pred a then (n, a) else go (n + 1) (f a)


findMinPower :: Int -> Int -> Vector Cell -> (Int, Int, Vector Cell)
findMinPower w h cells0 = go 4
  where
    len0 :: Int
    len0 = V.length $ V.filter isElf cells0

    noEnemies :: (Round, Vector Cell) -> Bool
    noEnemies = (== Incomplete) . fst

    isElfIntact :: Vector Cell -> Bool
    isElfIntact cells = len0 == V.length (V.filter isElf cells)

    go :: Int -> (Int, Int, Vector Cell)
    go ap =
      let (nRounds, (_, cells)) = repeatUntil noEnemies (simulateMove w h ap) (Initial, cells0)
      in  if isElfIntact cells then (ap, nRounds, cells) else go (ap + 1)


part2 :: IO ()
part2 = interactS "input15.txt" $ \lines -> do
  let cells0           = parse lines
      w                = length $ lines !! 0
      h                = length lines
      (ap, nRounds, cells) = findMinPower w h cells0
      fullRounds       = nRounds - 1
      sumOfhp          = hpSum cells
      out              = showCellsWithHp w cells
      outcome          = fullRounds * sumOfhp

  putStrLn out
  printf "Part2: Ap(%d) Full Rounds(%d)  Hp sum(%d) -> %d\n" ap fullRounds sumOfhp outcome
  putStrLn "\n"


test :: IO ()
test = do
  let inps =
        [ [ "#######"
          , "#.G...#"
          , "#...EG#"
          , "#.#.#G#"
          , "#..G#E#"
          , "#.....#"
          , "#######" ]
        , [ "#######"
          , "#G..#E#"
          , "#E#E.E#"
          , "#G.##.#"
          , "#...#E#"
          , "#...E.#"
          , "#######" ]
        , [ "#######"
          , "#E..EG#"
          , "#.#G.E#"
          , "#E.##E#"
          , "#G..#.#"
          , "#..E#.#"
          , "#######" ]
        , [ "#######"
          , "#E.G#.#"
          , "#.#G..#"
          , "#G.#.G#"
          , "#G..#.#"
          , "#...E.#"
          , "#######" ]
        , [ "#######"
          , "#.E...#"
          , "#.#..G#"
          , "#.###.#"
          , "#E#G#G#"
          , "#...#G#"
          , "#######" ]
        , [ "#########"
          , "#G......#"
          , "#.E.#...#"
          , "#..##..G#"
          , "#...##..#"
          , "#...#...#"
          , "#.G...G.#"
          , "#.....G.#"
          , "#########" ] ]

  forM_ inps $ \inp -> do
    let cells      = parse inp
        w          = length $ inp !! 0
        h          = length inp
        sims       = iterate (simulateMove w h 3) (Initial, cells)
        result     = fullNIncomplete sims
        indexes    = [0..] :: [Int]
        out        = showCellsWithHp w (snd . last $ result)
        (fullRounds, sumOfhp, outcome) = calculateOutcome result

    putStrLn out
    printf "Full Rounds: %d  Hp sum: %d -> %d\n" fullRounds sumOfhp outcome
    putStrLn "\n"

  -- forM_ (zip indexes result) $ \(i, (round, cells)) -> do
  --     let out = showCellsWithHp w cells
  --     printf "After %2d iteration Round: %s\n" i (show round)
  --     putStrLn out
  pure ()


testRounds :: IO ()
testRounds = do
  let inp =
        [ "#######"
        , "#.G...#"
        , "#...EG#"
        , "#.#.#G#"
        , "#..G#E#"
        , "#.....#"
        , "#######" ]
      cells      = parse inp
      w          = length $ inp !! 0
      h          = length inp
      moveCellss = iterate (simulateMove w h 3) (Incomplete, cells)
      indexes    = [0..47] :: [Int]

  forM_ (zip indexes moveCellss) $ \(i, (round, cells)) -> do
      let out = showCellsWithHp w cells
      printf "After %2d iteration Round: %s\n" i (show round)
      putStrLn out
      putStrLn "\n"

  pure ()


testMoves :: IO ()
testMoves = do
  let inp =
        [ "#########"
        , "#G..G..G#"
        , "#.......#"
        , "#.......#"
        , "#G..E..G#"
        , "#.......#"
        , "#.......#"
        , "#G..G..G#"
        , "#########" ]
      cells      = parse inp
      w          = length $ inp !! 0
      h          = length inp
      moveCellss = iterate (simulateMove w h 3) (Incomplete, cells)
      indexes    = [0..3] :: [Int]

  forM_ (zip indexes moveCellss) $ \(i, (round, cells)) -> do
      let out = showCells w cells
      printf "After %2d iteration\n" i
      putStrLn out
      putStrLn "\n"

  pure ()


testSearchMoveTarget :: IO ()
testSearchMoveTarget = do
  let inp =
        [ "#######"
        , "#E..G.#"
        , "#...#.#"
        , "#.G.#G#"
        , "#######" ]
      inp2 =
        [ "#######"
        , "#.E...#"
        , "#...?.#"
        , "#..?G?#"
        , "#######" ]
      start  = (1,1)
      start2 = (2,1)
      cells     = parse inp2
      src       = toIdx w start2
      w         = length $ inp2 !! 0
      h         = length inp2

      minDistTarget   = searchMoveTarget w h src cells
      (_, movedCells) = move w h src  minDistTarget cells
      cellsMinDistOut = showCellsWithPatch w cells [(minDistTarget, Custom '+')]
      movedCellsOut   = showCells w movedCells

  printf "Mindist Target: %s\n" (show $ fromIdx w  minDistTarget)

  print  " Distance Map "
  putStrLn cellsMinDistOut

  print  " After moving "
  putStrLn movedCellsOut
  pure ()


testFloodFill :: IO ()
testFloodFill = do
  let starts = [(4,2)] :: [(Int, Int)]
  let inps =
        [[ "#######"
        , "#.E...#"
        , "#.....#"
        , "#...G.#"
        , "#######" ]] :: [[String]]
      outs =
        [[ "#######"
        , "#4E212#"
        , "#32101#"
        , "#432G2#"
        , "#######" ]] :: [[String]]
      ios = zip3 starts inps outs :: [((Int,Int), [String], [String])]

  forM_ ios $ \(start, inp, out) -> do
    let cells    = parse inp
        w        = length $ inp !! 0
        h        = length inp
        distCells = floodFill w h (toIdx w start) cells
        out'     = intercalate "\n" out
    assert (showCells w distCells == out') $ pure ()
  pure ()


testRender :: IO ()
testRender = do
  let inp =
        [ "#######"
        , "#E..G.#"
        , "#...#.#"
        , "#.G.#G#"
        , "#######" ]
      cells = parse inp
      w     = length $ inp !! 0
      h     = length inp
  putStrLn $ showCells w cells
  pure ()
