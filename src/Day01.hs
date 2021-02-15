module Day01 
 ( test 
 , part1
 , part2
 ) where


import Lib
import Data.Set qualified as S
import Data.Set (Set)


parse :: String -> Int
parse [] = error "Empty number string"
parse (c:cs) = 
 case c of
   '+' -> read cs 
   '-' -> (-1) * (read cs)
   _   -> error "Invalid number string"


part1 :: IO ()
part1 = interactS "input01.txt" $ \ls -> do
  let ans = sum $ fmap parse ls
  printf "Part1: %d\n" ans


part2 :: IO ()
part2 = interactS "input01.txt" $ \ls -> do
  let xs = fmap parse ls
  case loop 0 (cycle xs) S.empty of
    Nothing -> printf "Part2: there's no repetition"
    Just ans -> printf "Part2: %d\n" ans


loop :: Int -> [Int] -> Set Int -> Maybe Int
loop _ []  _ = Nothing  
loop n (x:xs) s =  -- trace (show n ++ " " ++ show x) $ 
  let n' = x + n
  in  if S.member n' s then Just $ n'
      else loop n' xs (S.insert n' s)


test :: IO ()
test = do
  let inp1 = "+1"
  let inp2 = "-1"
  _ <- assert ( parse inp1 == 1 ) $ pure ()
  _ <- assert ( parse inp2 == (-1) ) $ pure ()
  pure ()
