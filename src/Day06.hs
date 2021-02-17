module Day06
  ( test
  , part1
  , part2
  ) where


import Lib
import Data.Map qualified as M
import Data.Map (Map)

{-
Part 1:
    fig. 1     fig. 2
    ../../....  \.........
    .A.././../  \A........
    /.././..//  .\\....\..
    .././../C.  ..\\....C.
    ./.D..//..  ...D\....\
    /./..E/...  \...\E....
    .B..//....  .B...\\...
    /.........  ..\...\\..
    ........./  ...\...\\.
    ........F.  ....\...F\


  Sorting points by distance from diagonals can be used
  for distinguishing infinite positions.

  In the fig.1, the infinite positions are A and F which
  has min,max distance for each.

  In the fig.2, the infinite positions are C and B.

  the slope of diagonals are 1 and -1 because it's (+/-)45 degree angle.

  TODO: ?? figure out why inversed??
  DONE: The y coordinate is upside down compared to conventional cartesian coordinate system.
        If we flip the figures, you will se the diagonals are inverted.
  positive diagonal equation: x + y + cp = 0 --- 1
  negative diagonal equation: x - y + cn = 0 --- 2

  We make the positive diagonal equation passes through top left point of
  the rectangle which contains all of the points. and negative diagonal
  equation passes through top right point of the rectangle

  top left point  = (min x in the all points, min y in the all points) --- 3
  top right point = (max x in the all points, min y in the all points) --- 4

  From equation 1 and 3, minx + miny + cp = 0 -> cp = - minx - miny
  From equation 2 and 4, maxx - miny + cn = 0 -> cn = - maxx + miny


  distance = |a * x + b * y + c| / sqrt(a^2 + b^2)
  distance = |a * x + b * y + c| / sqrt(a^2 + b^2)
  distance from positive diagonal = | x + y - minx - miny | / sqrt(2)
  distance from negative diagonal = | x - y - maxx + miny | / sqrt(2)

  points which has min max distance are the infinite area points

  For all points in the rectangle area, calculate distances for each points,
  and decide which is the closest one.

Part2:
  Seems likes the safe region only exists in the merged finite area.

  1. Merge finite areas including contested points.
  2. For all points in merged area, check the safety of the points and
     extract the region.
-}

data Pos = P
  { px :: Int
  , py :: Int
  , pc :: Char
  } deriving stock (Show, Eq)


instance Ord Pos where
  compare (P x1 y1 _) (P x2 y2 _) =
    if compare y1 y2 == EQ then compare x1 x2 else compare y1 y2


parsePos :: Char -> String -> Pos
parsePos c s =
  let [x, y] = splitOn ", " s
  in  P (read x) (read y) c



cornerCoords :: [Pos] -> (Int, Int, Int, Int)
cornerCoords ps =
  let minX = px $ minimumOn px ps
      maxX = px $ maximumOn px ps
      minY = py $ minimumOn py ps
      maxY = py $ maximumOn py ps
  in  (minX, maxX, minY, maxY)

{-
   distance = |a * x + b * y + c| / sqrt(a^2 + b^2)
   distance from positive diagonal = | x + y - minx - miny | / sqrt(2)
   distance from negative diagonal = | x - y - maxx + miny | / sqrt(2)
-}
distancePos :: Int -> Int -> Pos -> Int
distancePos minx miny (P px py _) = abs (px + py - minx - miny)


distanceNeg :: Int -> Int -> Pos -> Int
distanceNeg maxx miny (P px py _) = abs (px - py - maxx + miny)


sortPos :: Int -> Int -> [Pos] -> [Pos]
sortPos x y ps = sortOn (distancePos x y) ps


sortNeg :: Int -> Int -> [Pos] -> [Pos]
sortNeg x y ps = sortOn (distanceNeg x y) ps


infinitePos :: [Pos] -> [Pos]
infinitePos ps =
  let (minX, maxX, minY, maxY) = cornerCoords ps
      pps = sortPos minX minY ps
      nps = sortNeg maxX minY ps
      (lt, rb) = (head pps, last pps)
      (rt, lb) = (head nps, last nps)
  in  [lt, lb, rt, rb]


maxFiniteArea :: String -> String -> (Char, Int)
maxFiniteArea infChars renderedStr =
  let chars    = filter (isNotInfinite infChars) $ renderedStr
      counts   = fmap (\cs -> (head cs, length cs)) $ group chars
  in
      maximumOn snd $ fmap (\cs -> (head cs, length cs) ) $ group chars
  where
    isNotInfinite :: [Char] -> Char -> Bool
    isNotInfinite cs c = not $ c `elem` ('\n':'.':cs)


safeRegion :: Int -> [Char] -> [Pos] -> Map Pos Char -> [Pos]
safeRegion safeDistance infChars ps distanceMap =
  let
      maySafePs = M.keys $ M.filter isFiniteOrContested distanceMap :: [Pos]
      safePs    = filter isSafe maySafePs
  in
      safePs
  where
    isSafe :: Pos -> Bool
    isSafe sp = safeDistance > (sum $ fmap (distance sp) ps)
    isFiniteOrContested c = not $ c `elem` infChars

updateClosestPoints :: [Pos] -> (Int, Int, Map Pos Char)
updateClosestPoints ps =
  let (minX, maxX, minY, maxY) = cornerCoords ps
      rectPs = [ P x y '.' | y <- [minY .. maxY], x <- [minX .. maxX]] :: [Pos]
      distMap = M.fromList $ fmap (\rp -> (rp, minDistFrom ps rp)) rectPs
  in
      (maxX - minX + 1, maxY - minY + 1, distMap)


minDistFrom :: [Pos] -> Pos -> Char
minDistFrom ps p =
  case minimumsOn (distance p) ps of
    [minp] -> pc minp  -- sole winner in distance comparison
    _      -> '.'      -- there're contenders


distance :: Pos -> Pos -> Int
distance (P x1 y1 _) (P x2 y2 _) = abs (x1 - x2) + abs (y1 - y2)


renderDistMap :: Int -> Map Pos Char -> String
renderDistMap width m =
  let strs = chunksOf width $ M.elems m
  in  foldMap (<> "\n") strs


part1 :: IO ()
part1 = interactS "input06.txt" $ \lines -> do
  let labels = ['a'..'z'] <> ['A'..'Z']
  assert (length labels  >= length lines) $ pure ()
  let ps             = fmap (uncurry parsePos) (zip labels lines)
  let (w, _h, mDist) = updateClosestPoints ps
      output         = renderDistMap w mDist
      infChars       = fmap pc $ infinitePos ps
      maxArea        = maxFiniteArea infChars output
  printf "Part1: Max finite area: %s\n" (show maxArea)


part2 :: IO ()
part2 = interactS "input06.txt" $ \lines -> do
  let labels = ['a'..'z'] <> ['A'..'Z']
  assert (length labels  >= length lines) $ pure ()
  let ps             = fmap (uncurry parsePos) (zip labels lines)
      (w, _h, mDist) = updateClosestPoints ps
      infChars       = fmap pc $ infinitePos ps
      safePs         = safeRegion 10000 infChars ps mDist
  printf "Part2: Total region size: %2d\n" (length safePs)



testDist :: IO ()
testDist = do
  let inps =
        [ "1, 1"
        , "1, 6"
        , "8, 3"
        , "3, 4"
        , "5, 5"
        , "8, 9" ]
  let ps = fmap (\(c , s) -> parsePos c s) (zip ['A'..] inps)
  let (minX, maxX, minY, maxY) = cornerCoords ps
  printf "minX: %d , maxX: %d ,minY: %d ,maxY: %d\n"
         minX maxX minY maxY
  let posDist = fmap (\p -> (p, (distancePos minX minY p))) ps
  let negDist = fmap (\p -> (p, (distanceNeg maxX minY p))) ps

  -- A | B D | E C | F
  putStrLn "--- pos dist ---"
  mapM_ print $ posDist

  -- C | A E | D F | B
  putStrLn "--- neg dist ---"
  mapM_ print $ negDist


  -- A B C F --
  putStrLn "--- infinite pos test ---"
  mapM_ print $ infinitePos ps



test :: IO ()
test = do
  let inps =
        [ "1, 1"
        , "1, 6"
        , "8, 3"
        , "3, 4"
        , "5, 5"
        , "8, 9" ]
  putStrLn " --- parse test --- "
  let ps = fmap (\(c , s) -> parsePos c s) (zip ['A'..] inps)
  mapM_ print ps

  let (minX, maxX, minY, maxY) = cornerCoords ps
  printf "minX: %d , maxX: %d ,minY: %d ,maxY: %d\n"
         minX maxX minY maxY

  let positivelysorted = sortPos minX minY ps
  let negativelysorted = sortNeg maxX minY ps
  putStrLn " --- positive sort test --- "
  mapM_ print $ positivelysorted
  putStrLn " --- negative sort test --- "
  mapM_ print $ negativelysorted


  putStrLn " --- distance Map test  --- "
  let (width, height, distanceMap) = updateClosestPoints ps
      output = renderDistMap width distanceMap
  printf "width: %2d, height %2d\n" width height
  -- putStrLn output
  mapM_ print $ (M.toList distanceMap)

  putStrLn " --- maxFiniteArea test  --- "
  let infChars = fmap pc $ infinitePos ps
  printf "Infinite chars : %s\n" infChars


  let finiteDistMap = M.filter (\c -> c /= '.' && (not $ c `elem` infChars)) distanceMap
  let safeRegionCandidateMap  = M.filter (\c -> not $ c `elem` infChars) distanceMap
  -- mapM_ print $ (M.toList finiteDistMap)
  mapM_ print $ (M.toList safeRegionCandidateMap)
  -- let maxArea = maxFiniteArea infChars output
  -- mapM_ print $ (fmap pc $ infinitePos ps)
  -- printf "Max finite area: %s\n" (show maxArea)

  putStrLn " --- safe region test  --- "
  let safePs = safeRegion 32 infChars ps distanceMap
  mapM_ print $ safePs
  printf "total region size: %2d\n" (length safePs)
  pure ()
