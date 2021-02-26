module Day12
  ( test
  ) where

{- That's it for today. -}

import Lib
import Data.Vector qualified as V
import Data.Vector (Vector)


type Rule = (Vector Char, Char)


parseState :: String -> Vector (Int, Char)
parseState s =
  let [_, rest] = splitOn ": " s
      len       = length rest
      padding   = 200
  in  V.generate (padding * 2 + len) (\i -> (i - padding, toChar padding len rest i))
  where
    toChar padding len xs i =
      if i < padding || i > (len + padding - 1)
      then '.' else (xs !! (i - padding))


parseRule :: String -> (Vector Char, Char)
parseRule s =
  let [l, r] = splitOn " => " s
  in  (V.generate (length l) (\i -> l !! i), head r)


parse :: [String] -> (Vector (Int, Char), [Rule])
parse ss =
  let (stateSS, ruleSS) = (head ss, tail . tail $ ss)
      vState            = parseState stateSS
      rules             = fmap parseRule ruleSS
  in  (vState, rules)


{-
   There's must be enough space for spreading pot locations.
-}

nextGen :: [Rule] -> Vector (Int, Char) -> Vector (Int, Char)
nextGen rules vState =
  V.imap (\i (n, c) -> (n, applyRules vState rules i c)) vState


applyRules :: Vector (Int, Char) -> [Rule] -> Int -> Char -> Char
applyRules vState rules stateIdx stateChar =
  if stateIdx < 2 || stateIdx >= (V.length vState - 2)
  then stateChar
  else fromMaybe '.' $ asum $ fmap (applyRule vState stateIdx) rules
  where
    applyRule :: Vector (Int, Char) -> Int -> Rule -> Maybe Char
    applyRule vState stateIdx (vRule, ruleChar) =
      let matchRule = all (\i -> vRule V.! (i + 2) == snd (vState V.! (stateIdx + i))) [-2..2]
      in  if matchRule then Just ruleChar else Nothing


showState :: Vector (Int, Char) -> String
showState vState = show $ fmap snd vState


part1 :: IO ()
part1 = interactS "input12.txt" $ \lines -> do
  let (vState, rules) = parse lines
  let generations     = take 21 $ iterate (nextGen rules) vState
      idxes           = [0..] :: [Int]
  let totalTrees      = sum $ fmap fst $ V.filter ((== '#') . snd) (last generations)
  printf "Part1: Total trees: %d\n" totalTrees
  -- mapM_ (\(i, g) -> printf "S(%2d)| %s\n" i (showState g)) (zip idxes generations)


nTreeSumN :: Integer -> Integer -> Integer
nTreeSumN nTreeSum100 n =
  let diffN = n - 100
  in  diffN * 32 + nTreeSum100


part2 :: IO ()
part2 = interactS "input12.txt" $ \lines -> do
  let (vState, rules) = parse lines
      generations     = take 101 $ iterate (nextGen rules) vState
      gen100          = last generations
      nTreeSum100     = sum $ fmap fst $ V.filter ((== '#') . snd) gen100
  printf "Part2: Sum of tree numbers after 50000000000: %d\n" (nTreeSumN (fromIntegral nTreeSum100) 50000000000)


test :: IO ()
test = do
  let inp =
        [ "initial state: #..#.#..##......###...###"
        , ""
        , "...## => #"
        , "..#.. => #"
        , ".#... => #"
        , ".#.#. => #"
        , ".#.## => #"
        , ".##.. => #"
        , ".#### => #"
        , "#.#.# => #"
        , "#.### => #"
        , "##.#. => #"
        , "##.## => #"
        , "###.. => #"
        , "###.# => #"
        , "####. => #" ]
  -- KEKW, correctly copying example in 2021.

  let (vState, rules) = parse inp
  printf "S| %s\n" (show vState)
  mapM_ (\(vRule, c) -> printf "R| %s => %c\n" (show vRule) c) rules

  let generations = take 21 $ iterate (nextGen rules) vState
      idxes  = [0..] :: [Int]
  mapM_ (\(i, g) -> printf "S(%2d)| %s\n" i (showState g)) (zip idxes generations)
  let totalTrees = sum $ fmap fst $ V.filter ((== '#') . snd) (last generations)
  printf "Total trees: %d\n" totalTrees
  pure ()
