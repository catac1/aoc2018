module Day17
  ( test
  , part1
  , part2
  ) where


import Lib
import Data.Array.Unboxed
import Data.Array.MArray
import Data.Array.ST
import GHC.Generics


data Line
  = V Int [Int]
  | H Int [Int]
  deriving stock Show


data Pos = P
  { px :: Int
  , py :: Int
  } deriving stock (Eq, Ix, Show, Generic)


instance Ord Pos where
  compare (P x1 y1) (P x2 y2) = let cmpy = compare y1 y2 in
    if cmpy == EQ then compare x1 x2 else cmpy

instance Grouping Pos


movL :: Pos -> Pos
movL (P x y) = P (x - 1) y

movR :: Pos -> Pos
movR (P x y) = P (x + 1) y

movU :: Pos -> Pos
movU (P x y) = P x (y - 1)

movD :: Pos -> Pos
movD (P x y) = P x (y + 1)


parse :: [String] -> [Line]
parse ss = fmap parseLine ss


parseLine :: String -> Line
parseLine s =
  let [a  , b   ] = splitOn ", " s
      [xy1, val1] = splitOn "="  a
      [xy2, rng ] = splitOn "="  b
      [r1 , r2  ] = splitOn ".." rng
   in case (xy1, xy2) of
        ("x", "y") -> V (read val1) [read r1 .. read r2]
        ("y", "x") -> H (read val1) [read r1 .. read r2]
        _          -> error "input format error"


mkGround :: [Line] -> (Int, Int, UArray Pos Char)
mkGround lines =
  let arr0 = listArray bnds (repeat ' ')
      ps   = concatMap line2points lines
      well = (P 500 0, '+')
      minX = px $ minimumOn px ps
      minY = py $ minimumOn py ps
      maxX = px $ maximumOn px ps
      maxY = py $ maximumOn py ps
      bnds = (P (minX -2) 0, P (maxX + 2) (maxY + 2) )
   in -- (trace $ foldMap (<> "\n") (fmap show ps) ) $
      (minY, maxY, arr0 // (well : fmap (,'#') ps))


line2points :: Line -> [Pos]
line2points line = case line of
  V x ys -> [ P x y | y <- ys ]
  H y xs -> [ P x y | x <- xs ]


renderGround :: UArray Pos Char -> String
renderGround arr =
  let (P minX minY, P maxX maxY) = bounds arr
      w     = maxX - minX + 1
      lines = chunksOf w       $
              fmap snd         $
              sortOn fst       $
              assocs arr
      labels = fmap (printf "%4d| ") ([0 .. length lines -1] :: [Int])
   in intercalate "\n" $ zipWith (<>) labels lines


data Drop = Bottom Pos | ZeroLen | Infinity deriving stock Show


dropWater :: forall s. Int -> STUArray s Pos Char -> Pos -> ST s (Drop, STUArray s Pos Char)
dropWater maxY arr p0 = go p0 [] >>= \case
  (ZeroLen , _ ) ->                   pure (ZeroLen , arr)
  (Infinity, ps) -> drawDrop p0 ps *> pure (Infinity, arr)
  (d       , ps) -> drawDrop p0 ps *> pure (d       , arr)
  where
    go :: Pos -> [Pos] -> ST s (Drop, [Pos])
    go p acc = do
      let pn = movD p
      c <- readArray arr pn
      if | py pn == maxY + 1    -> pure (Infinity, acc)
         | c /= '#' && c /= '~' -> go pn (pn : acc)
         | otherwise            ->
           let drop = if null acc then ZeroLen else Bottom (head acc)
            in pure (drop, acc)

    drawDrop :: Pos -> [Pos] -> ST s ()
    drawDrop p ps = do
      forM_ ((p, 'x') : (fmap (, '|') ps)) $ \(p, c) -> writeArray arr p c


data Fill = ContFill | StopFill

instance Semigroup Fill where
  ContFill <> ContFill = ContFill
  _        <> _        = StopFill


fillWater :: forall s. STUArray s Pos Char -> Pos -> ST s ([Pos], STUArray s Pos Char)
fillWater arr p0 = goV p0 >>= pure . (, arr)
  where
    goV :: Pos -> ST s [Pos]
    goV p = do
      (fillL, overPsL, pls) <- goH movL p []
      (fillR, overPsR, prs) <- goH movR p []
      let dropPs = nub $ overPsL <> overPsR
          fillLR = fillL <> fillR
          ps     = pls <> (p : prs)
      drawSpread fillLR ps
      case fillLR of
        ContFill -> goV (movU p)
        StopFill -> pure dropPs

    goH :: (Pos -> Pos) -> Pos -> [Pos] -> ST s (Fill, [Pos], [Pos])
    goH f p acc = do
      let pn = f p
          pd = movD p
      cn <- readArray arr pn
      cd <- readArray arr pd
      case (cn, cd) of
        ('x', _  ) -> pure (StopFill, [] , [] )
        ('#', _  ) -> pure (ContFill, [] , acc)
        (_  , ' ') -> pure (StopFill, [p], acc)
        _          -> goH f pn (pn : acc)

    drawSpread :: Fill -> [Pos] -> ST s ()
    drawSpread fill ps =
      let spchr = case fill of ContFill -> '~'; StopFill -> '*'
       in forM_ (fmap (, spchr) ps) $ \(p, c) -> writeArray arr p c


sim :: forall s. Int -> Pos -> STUArray s Pos Char -> ST s (STUArray s Pos Char)
sim maxY p0 arr = go [p0]
  where
    go :: [Pos] -> ST s (STUArray s Pos Char)
    go []     = pure arr
    go (p:ps) = fmap fst (dropWater maxY arr p) >>= \case
      Infinity       -> go ps
      ZeroLen        -> go ps
      (Bottom btmPs) -> -- trace ("bottom -> " <> (show . py $ btmPs)) $
        fillWater arr btmPs >>= go . (ps <>) . fst


countWaterTile :: UArray Pos Char -> Int -> Int -> [Char] -> Int
countWaterTile arr minY maxY validCs =
  length $
  filter (\(p, c) -> isWaterTile c && inYRange (py p)) $
  assocs arr
  where
    isWaterTile c = c `elem` validCs
    inYRange y = y >= minY && y <= maxY


part1 :: IO ()
part1 = interactS "input17.txt" $ \inps -> do
  let lines                = parse inps
      (minY, maxY, ground) = mkGround lines
      wellPs               = P 500 1
      ground'              = runSTUArray $ thaw ground >>= sim maxY wellPs
      output               = renderGround ground'
      nWaterCells          = countWaterTile ground' minY maxY ['x', '|', '~', '*']

  -- print " --- Ground -- "
  -- putStrLn output
  -- printf "Minimum Y: %4d, Maximum Y: %4d\n" minY maxY
  printf "Part1: Number of water cells %d\n" nWaterCells


part2 :: IO ()
part2 = interactS "input17.txt" $ \inps -> do
  let lines                = parse inps
      (minY, maxY, ground) = mkGround lines
      wellPs               = P 500 1
      ground'              = runSTUArray $ thaw ground >>= sim maxY wellPs
      output               = renderGround ground'
      nWaterCells          = countWaterTile ground' minY maxY ['~']

  -- printf "Minimum Y: %4d, Maximum Y: %4d\n" minY maxY
  printf "Part2: Number of water cells will be retained %d\n" nWaterCells



test :: IO ()
test = do
  -- let inp =
  --       [ "x=495, y=4..7"
  --       , "y=7, x=495..501"
  --       , "x=501, y=4..7"
  --       , "x=498, y=5..5"
  --       , "x=506, y=1..2"
  --       , "x=493, y=10..13"
  --       , "x=504, y=11..13"
  --       , "y=13, x=493..504" ]
  let inp =
        [ "x=495, y=2..7"
        , "y=7, x=495..501"
        , "x=501, y=3..7"
        , "x=498, y=2..4"
        , "x=506, y=1..2"
        , "x=498, y=10..13"
        , "x=504, y=10..13"
        , "y=13, x=498..504" ]
      lines                = parse inp
      (minY, maxY, ground) = mkGround lines
      wellPs               = P 500 1
      ground'              = runSTUArray $ thaw ground >>= sim maxY wellPs
      output               = renderGround ground'
      nWaterCells          = countWaterTile ground' minY maxY ['~', '*', '|', 'x']

  print " --- Ground -- "
  putStrLn output
  printf "Minimum Y: %4d, Maximum Y: %4d\n" minY maxY
  printf "Number of water cells %d\n" nWaterCells


testDrop :: IO ()
testDrop = do
  let inp =
        [ "x=495, y=2..7"
        , "y=7, x=495..501"
        , "x=501, y=3..7"
        , "x=498, y=2..4"
        , "x=506, y=1..2"
        , "x=498, y=10..13"
        , "x=504, y=10..13"
        , "y=13, x=498..504" ]
      lines                = parse inp
      (minY, maxY, ground) = mkGround lines
      ground'              = runSTUArray $ thaw ground >>= \g -> fmap snd (dropWater maxY g (P 500 0))
      d                    = runST $ thaw ground >>= \g -> fmap fst (dropWater maxY g (P 500 0))
      output               = renderGround ground'

  print " --- Ground -- "
  putStrLn output

  print " --- Drop -- "
  print d


testFillWater :: IO ()
testFillWater = do
  let inp =
        [ "x=495, y=2..7"
        , "y=7, x=495..501"
        , "x=501, y=3..7"
        , "x=498, y=2..4"
        , "x=506, y=1..2"
        , "x=498, y=10..13"
        , "x=504, y=10..13"
        , "y=13, x=498..504" ]
      lines                = parse inp
      (minY, maxY, ground) = mkGround lines
      ground'              = runSTUArray $ thaw ground >>= \g -> fmap snd (fillWater g (P 500 6))
      ps                   = runST $ thaw ground >>= \g -> fmap fst (fillWater g (P 500 6))
      output               = renderGround ground'

  print " --- Ground -- "
  putStrLn output

  print " --- Overflows -- "
  mapM_ print ps


testRender :: IO ()
testRender = do
  let inp =
        [ "x=495, y=2..7"
        , "y=7, x=495..501"
        , "x=501, y=3..7"
        , "x=498, y=2..4"
        , "x=506, y=1..2"
        , "x=498, y=10..13"
        , "x=504, y=10..13"
        , "y=13, x=498..504" ]
      lines                = parse inp
      (minY, maxY, ground) = mkGround lines
      output               = renderGround ground
  mapM_ print lines
  putStrLn output
  pure ()
