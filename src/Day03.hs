module Day03
  ( test
  ) where


import Lib
import Data.Vector.Mutable qualified as MV
import Data.Vector.Mutable (MVector)
import Data.Vector qualified as V
import Data.Vector (Vector)
import Data.Map qualified as M
import Data.Map (Map)


maxX :: Int
maxX = 1000
-- maxX = 10

maxY :: Int
maxY = 1000
-- maxY = 10


data Rect = R
  { rx :: Int
  , ry :: Int
  , rw :: Int
  , rh :: Int
  , rn :: Int
  } deriving stock (Show, Eq)


data Cell = Single Int | Overlap | Empty
  deriving stock (Show, Eq)


setup :: ST s (MVector s Cell)
setup = do
  mv <- MV.new (maxX * maxY)
  forM_ [0..maxX * maxY - 1] $ \i -> do
    MV.write mv i Empty
  pure mv


writeRect :: Rect -> MVector s Cell -> ST s ()
writeRect (R x y w h n) mv = do
  forM_ [y..y + h - 1] $ \iy -> do
    forM_ [x..x + w - 1] $ \ix ->  do
      writeCell mv ix iy n


writeCell :: MVector s Cell -> Int -> Int -> Int -> ST s ()
writeCell mv x y n = do
  MV.read mv (toIdx x y) >>= \case
    Empty    -> MV.write mv (toIdx x y) (Single n)
    Single _ -> MV.write mv (toIdx x y) Overlap
    _ -> pure ()


toIdx :: Int -> Int -> Int
toIdx x y = y * maxX + x


rectToCells :: [Rect] -> [Cell]
rectToCells rects  = runST $ do
  mv <- setup
  forM_ rects $ \r -> do
    writeRect r mv
  v <- V.freeze mv
  pure (V.toList v)


countOverlap :: [Cell] -> Int
countOverlap xs = length $ filter (== Overlap) xs


toCellMap :: [Cell] -> Map (Int, Int) Cell
toCellMap cells =
  let xys = [(x, y) | y <- [0..maxY - 1], x <- [0..maxX - 1]]
  in  M.fromList $ zip xys cells


isNotOverlap :: Map (Int, Int) Cell -> Rect -> Bool
isNotOverlap m (R rx ry rw rh n) =
  let xysInR = [(x, y) | y <- [ry..ry + rh - 1], x <- [rx..rx + rw -1]]
  in  all thisCell xysInR
  where
     thisCell xy = case M.lookup xy m of
       Nothing -> error "Impossible lookup"
       Just cell -> case cell of
         Single n' -> n' == n
         Empty     -> False
         Overlap   -> False


parse :: String -> Rect
parse s =
  let [tileN, xyRest ] = splitOn " @ " s
      [   xy,     wh ] = splitOn ": " xyRest
      [    x,      y ] = splitOn " ," xy
      [    w,      h ] = splitOn "x" wh
  in  R (read x) (read y) (read w) (read h) (read . tail $ tileN)


part1 :: IO ()
part1 = interactS "input03.txt" $ \ls -> do
  let rects = fmap parse ls
      cells = rectToCells rects
      nOverlap = countOverlap cells
  printf "Part1: %d\n" nOverlap


part2 :: IO ()
part2 = interactS "input03.txt" $ \ls -> do
  let rects    = fmap parse ls
      cellMap  = toCellMap . rectToCells $ rects
      theCell  = filter snd $ fmap (\r -> (r, isNotOverlap cellMap r)) rects
  case theCell of
    [] -> printf "Part2: there's no non-overlapping claim\n"
    [(r, _)] -> printf "Part2: %d\n" (rn r)
    _        -> printf "Part2: ballocks more than one claims are not overlapping.\n"


test :: IO ()
test = do
  let inps =
        [ "#1 @ 1,3: 4x4"
        , "#2 @ 3,1: 4x4"
        , "#3 @ 5,5: 2x2" ]

  let rects = fmap parse inps
  let cells  = rectToCells rects
  let nOverlap = countOverlap cells
  print nOverlap

  let cellMap  = toCellMap cells
  let results = fmap (isNotOverlap cellMap) rects
  mapM_ print $ results
  mapM_ print $ M.toList $ M.filter (== Overlap) cellMap
  pure ()
