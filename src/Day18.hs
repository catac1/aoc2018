module Day18
  ( test
  , part1
  , part2
  ) where


import Lib
import Data.Array
import Data.Functor.Identity
import Data.Functor.Rep
import Data.Distributive
import Data.Ix
import Control.Comonad.Representable.Store
import Control.Comonad
import Data.Set qualified as S
import Data.Set (Set)
import GHC.TypeLits
import Data.Proxy


data Pos = P { px :: Int, py :: Int }
  deriving stock (Show, Eq, Ix)

instance Ord Pos where
  compare (P x1 y1) (P x2 y2) = let cmpy = compare y1 y2 in
    if cmpy == EQ then compare x1 x2 else cmpy


adjPos :: Int -> Pos -> [Pos]
adjPos n (P x y) =
  filter (\(P x y) -> x >= 1 && y >= 1 && x <= n && y <= n) $
  [ P (x + dx) (y + dy)
  | dx <- [-1, 0, 1]
  , dy <- [-1, 0, 1]
  , dx /= 0 || dy /= 0 ]


newtype Container (n :: Nat) a = C (Array Pos a)
  deriving newtype Functor

instance KnownNat n => Distributive (Container n) where
  distribute = distributeRep

instance KnownNat n => Representable (Container n) where
  type Rep (Container n) = Pos
  index (C arr) p= arr ! p
  tabulate f =
    let n = fromIntegral $ natVal (Proxy @n)
        r = (P 1 1, P n n)
     in C $ array r [ (p, f p) | p <- range r ]


type OutSkirts n a = Store (Container n) a

instance (KnownNat n, Eq a) => Eq (OutSkirts n a) where
  out1 == out2 = unStore out1 == unStore out2

instance (KnownNat n, Ord a) => Ord (OutSkirts n a) where
  out1 `compare` out2 = unStore out1 `compare` unStore out2

unStore :: Store (Container n) a -> Array Pos a
unStore (StoreT (Identity (C arr)) _) = arr


step :: KnownNat n => OutSkirts n Char -> OutSkirts n Char
step = extend rule


rule :: forall n. KnownNat n => OutSkirts n Char -> Char
rule outskirts = case center of
  '.' -> if countChar '|' >= 3 then '|' else center
  '|' -> if countChar '#' >= 3 then '#' else center
  '#' -> if countChar '#' >= 1 && countChar '|' >= 1 then '#' else '.'
  _   -> error "Invalid char"
  where
    n           = fromIntegral $ natVal (Proxy @n)
    center      = extract outskirts
    adjs        = experiment (adjPos n) outskirts
    countChar c = length . filter (== c) $ adjs


renderOutSkirts :: forall n. KnownNat n => OutSkirts n Char -> String
renderOutSkirts out =
  intercalate "\n" $
  chunksOf (fromIntegral $ natVal (Proxy @n)) $
  fmap snd $
  sortOn fst $
  assocs $
  unStore out


calcScore :: OutSkirts n Char -> (Int, Int, Int)
calcScore (StoreT (Identity (C arr)) _) =
  let chars       = elems arr
      nLumberYard = length . filter (== '#') $ chars
      nTrees      = length . filter (== '|') $ chars
   in (nLumberYard, nTrees, nLumberYard * nTrees)


parse :: forall n. KnownNat n => [String] -> OutSkirts n Char
parse ss =
  let n   = fromIntegral $ natVal (Proxy @n)
      r   = (P 1 1, P n n)
      arr = array r (zip (sort $ range r) (concat ss))
   in StoreT (Identity (C arr)) (P 1 1)


part1 :: IO ()
part1 = interactS "input18.txt" $ \lines -> do
  let outskirts = parse @50 lines
      minutes   = 10
      outputs   = take (minutes + 1) $ iterate step outskirts
      (l, t, s) = calcScore (last outputs)
      idexes    = [0..] :: [Int]
  forM_ (zip idexes outputs) $ \(i, out) -> do
    printf "\nAfter %2d minutes\n" i
    putStrLn $ renderOutSkirts out

  printf "Part1: LumberYars: %3d, Wood acres: %3d => %8d\n" l t s


detectDuplicate :: KnownNat n => OutSkirts n Char -> OutSkirts n Char
detectDuplicate out0 = go out0 (S.singleton out0)
  where
    go out s = let out' = step out in
      if S.member out' s then out' else go out' (S.insert out' s)


part2 :: IO ()
part2 = interactS "input18.txt" $ \lines -> do
  let outskirts  = parse @50 lines
      minutes    = 1000000000
      outputs    = iterate step outskirts
      outskirts' = detectDuplicate outskirts
      (uniques, rest) = break (== outskirts') outputs
      cycles     = outskirts' : (takeWhile (/= outskirts') $ tail rest)
      scoreCycle = fmap calcScore cycles
      nUnique    = length uniques
      nCycle     = length cycles
      (l, t, s)  = scoreCycle !! ((minutes - nUnique) `mod` nCycle)

  putStrLn $ renderOutSkirts outskirts'
  printf "Unique Outskirts: %3d, Repeat outskirts cycle: %2d\n" nUnique nCycle
  printf "Part2: After %8d minutes =>  LumberYars: %6d, Wood acres: %6d => %12d\n" minutes l t s


-- NOTE: We need to predefine min/max values due to limitation of Representable instance.
-- This makes the typeclass instance inflexible with respect to the size of the underlying conainter.
-- DONE: Typelevel programming to encode container size at compiler time.

test :: IO ()
test = do
  let inps =
        [ ".#.#...|#."
        , ".....#|##|"
        , ".|..|...#."
        , "..|#.....#"
        , "#.#|||#|#|"
        , "...#.||..."
        , ".|....|..."
        , "||...#|.#|"
        , "|.||||..|."
        , "...#.|..|." ]
  let outskirts = parse @10 inps
      outputs   = take 11 $ iterate step outskirts
      (y, t, s) = calcScore (last outputs)
      idexes    = [0..] :: [Int]

  forM_ (zip idexes outputs) $ \(i, out) -> do
    printf "After %2d minutes\n" i
    putStrLn $ renderOutSkirts out
  printf "LumberYars: %3d, Wood acres: %3d => %8d\n" y t s
  pure ()


testParse :: IO ()
testParse = do
  let inps =
        [ ".#.#...|#."
        , ".....#|##|"
        , ".|..|...#."
        , "..|#.....#"
        , "#.#|||#|#|"
        , "...#.||..."
        , ".|....|..."
        , "||...#|.#|"
        , "|.||||..|."
        , "...#.|..|." ]
  let arr   = unStore $ parse @10 inps
      inps' = chunksOf 10 $ fmap snd $ sortOn fst $ assocs arr
  assert (inps == inps') $ pure ()
  pure ()
