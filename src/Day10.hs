module Day10
  ( test
  , part1
  , part2
  ) where


import Lib
import Data.Map qualified as M
import Data.Map (Map)


data Pos = P
  { px :: Int
  , py :: Int
  } deriving stock (Eq)


instance Show Pos where
  show (P x y) = printf "P| %3d %3d" x y


data Vel = V
  { vx :: Int
  , vy :: Int
  } deriving stock (Eq)


instance Show Vel where
  show (V x y) = printf "V| %3d %3d" x y



parse :: String -> (Pos, Vel)
parse s0 =
  let [posStr, velStr]   = splitOn " v" s0
      (posStr', velStr') = (strip posStr, strip velStr)
      [pxStr, pyStr]     = splitOnComma posStr'
      [vxStr, vyStr]     = splitOnComma velStr'
  in  (P (read pxStr) (read pyStr), V (read vxStr) (read vyStr))
  where
    strip s = tail $ takeWhile (/= '>') $ dropWhile (/= '<') s
    splitOnComma s = splitOn ", " s


updatePos :: (Pos, Vel) -> (Pos, Vel)
updatePos ((P px py), v@(V vx vy)) = (P (px + vx) (py + vy), v)


advanceTime :: [(Pos, Vel)] -> [(Pos, Vel)]
advanceTime pvs = fmap updatePos pvs


afterNSec :: Int -> [(Pos, Vel)] -> [(Pos, Vel)]
afterNSec n pvs = head $ drop n $ iterate advanceTime pvs


guessMessage :: [(Pos, Vel)] -> (Int, [(Pos, Vel)])
guessMessage pvs0 =
  let area0      = area pvs0
      candidates = takeWhile (\(_, a) -> a < area0)
                   $ fmap (\pvs -> (pvs, area pvs))
                   $ tail $ iterate advanceTime pvs0
      minPs      = fst $ minimumOn snd $ candidates
      sec        = fromMaybe 0 $ findIndex ((== minPs) . fst) candidates
  in  (sec + 1, minPs)


instance Ord Pos where
  compare (P x1 y1) (P x2 y2) = let cmpy = compare y1 y2 in
    if cmpy == EQ then compare x1 x2 else cmpy


area :: [(Pos, Vel)] -> Int
area pvs =
  let ps    = fmap fst pvs :: [Pos]
      cells = M.fromList
              $ fmap (\(P x y) -> (P (x - minX) (y - minY), '#') ) ps
      minX = px $ minimumOn px ps :: Int
      minY = py $ minimumOn py ps :: Int
      maxX = px $ maximumOn px ps :: Int
      maxY = py $ maximumOn py ps :: Int
  in  (maxX - minX + 1) * (maxY - minY + 1)


renderPV :: [(Pos, Vel)] -> String
renderPV pvs =
  let ps    = fmap fst pvs :: [Pos]
      cells = M.fromList
              $ fmap (\(P x y) -> (P (x - minX) (y - minY), '#') ) ps
      minX = px $ minimumOn px ps :: Int
      minY = py $ minimumOn py ps :: Int
      maxX = px $ maximumOn px ps :: Int
      maxY = py $ maximumOn py ps :: Int
      background = M.fromList $
                   [ (P x y,  '.')
                   | y <- [0 .. maxY - minY]
                   , x <- [0 .. maxX - minX] ]
      width  = maxX - minX + 1
      merged = M.elems $ cells `M.union` background
  in  foldMap (<> "\n") $ chunksOf width merged


part1 :: IO ()
part1 = interactS "input10.txt" $ \lines -> do
  let posNvel   = fmap parse lines
      (_sec, ps) = guessMessage posNvel
  printf "Part1: The scret message is...\n"
  putStrLn $ renderPV ps


part2 :: IO ()
part2 = interactS "input10.txt" $ \lines -> do
  let posNvel   = fmap parse lines
      (sec, _ps) = guessMessage posNvel
  printf "Part2: After %d seconds, the message appears\n" sec


test :: IO ()
test = do
  let inp =
        [ "position=< 9,  1> velocity=< 0,  2>"
        , "position=< 7,  0> velocity=<-1,  0>"
        , "position=< 3, -2> velocity=<-1,  1>"
        , "position=< 6, 10> velocity=<-2, -1>"
        , "position=< 2, -4> velocity=< 2,  2>"
        , "position=<-6, 10> velocity=< 2, -2>"
        , "position=< 1,  8> velocity=< 1, -1>"
        , "position=< 1,  7> velocity=< 1,  0>"
        , "position=<-3, 11> velocity=< 1, -2>"
        , "position=< 7,  6> velocity=<-1, -1>"
        , "position=<-2,  3> velocity=< 1,  0>"
        , "position=<-4,  3> velocity=< 2,  0>"
        , "position=<10, -3> velocity=<-1,  1>"
        , "position=< 5, 11> velocity=< 1, -2>"
        , "position=< 4,  7> velocity=< 0, -1>"
        , "position=< 8, -2> velocity=< 0,  1>"
        , "position=<15,  0> velocity=<-2,  0>"
        , "position=< 1,  6> velocity=< 1,  0>"
        , "position=< 8,  9> velocity=< 0, -1>"
        , "position=< 3,  3> velocity=<-1,  1>"
        , "position=< 0,  5> velocity=< 0, -1>"
        , "position=<-2,  2> velocity=< 2,  0>"
        , "position=< 5, -2> velocity=< 1,  2>"
        , "position=< 1,  4> velocity=< 2,  1>"
        , "position=<-2,  7> velocity=< 2, -2>"
        , "position=< 3,  6> velocity=<-1, -1>"
        , "position=< 5,  0> velocity=< 1,  0>"
        , "position=<-6,  0> velocity=< 2,  0>"
        , "position=< 5,  9> velocity=< 1, -2>"
        , "position=<14,  7> velocity=<-2,  0>"
        , "position=<-3,  6> velocity=< 2, -1>" ]
      posNvel = fmap parse inp

  mapM_ print posNvel
  let (sec, pvs) = guessMessage posNvel
  print sec
  putStrLn $ renderPV pvs
  pure ()
