module Day02
  ( test1
  , test2
  ) where


import Lib
import Data.Map qualified as M
import Data.Map (Map)


countExactLetter :: String -> Map Char Int
countExactLetter s0 = go s0 M.empty
  where
    go []     m  = m
    go (c:cs) m  = go cs (M.alter alterf c m)

    alterf :: Maybe Int -> Maybe Int
    alterf mi = case mi of
      Nothing -> Just 1
      Just i  -> Just (i + 1)


toValidCount :: Map Char Int -> (Int, Int)
toValidCount m =
  let nTwo = M.size $ M.filter (== 2) m
      nThree = M.size $ M.filter (== 3) m
  in  (toCount nTwo, toCount nThree)
  where
    toCount n = if n > 0 then 1 else 0


checksum :: [(Int, Int)] -> Int
checksum tups =
  let  (two, three) = foldl accf (0,0) tups
  in   two * three
  where
    accf (accTwo, accThree) (a, b) = (a + accTwo, b + accThree)


part1 :: IO ()
part1 = interactS "input02.txt" $ \ls -> do
  let ans = checksum $ fmap (toValidCount . countExactLetter) ls
  printf "Part1: %d\n" ans


countDiff :: String -> String -> (String, String, Int)
countDiff s1 s2 =
  (s1, s2, length $ filter (uncurry (/=)) $ zip s1 s2)


commonPart :: String -> String -> String
commonPart s1 s2 = fmap fst $ filter (uncurry (==)) $ zip s1 s2


part2 :: IO ()
part2 = interactS "input02.txt" $ \ls -> do
  let results  = filter (\(_, _, i) -> i == 1)
                 $ [countDiff s1 s2 | s1 <- ls, s2 <- ls, s1 < s2]
  case results of
    [(sa, sb, _)] -> printf "Part2: %s \n" (commonPart sa sb)
    []         -> printf "Part2: there's no matching IDs\n"
    _          -> printf "Part2: there's multiple matches\n"


test2 :: IO ()
test2 = do
  let inp =
        [ "abcde"
        , "fghij"
        , "klmno"
        , "pqrst"
        , "fguij"
        , "axcye"
        , "wvxyz" ]
  print $ countDiff (inp !! 1) (inp !! 4)
  print $ countDiff (inp !! 0) (inp !! 5)
  pure ()



test1 :: IO ()
test1 = do
  let inps =
        [ "abcdef"
        , "bababc"
        , "abbcde"
        , "abcccd"
        , "aabcdd"
        , "abcdee"
        , "ababab" ]
  let ms  = fmap (countExactLetter) inps
  forM_ ms $ \m -> do
    print "-----------"
    -- mapM_ print (M.toList m)
    print (toValidCount m)

  let ans = checksum $ fmap (toValidCount . countExactLetter) inps
  printf "Part1: %d\n" ans
  pure ()
