module Day05
  ( test
  ) where


import Lib


data Zipper a = Z [a] [a]

instance Show a => Show (Zipper a) where
  show (Z []     []    ) = printf "Empty Zipper"
  show (Z (c:ls) []    ) = printf "%s (%s) []" (show (reverse ls)) (show c)
  show (Z []     (c:rs)) = printf "[] (%s) %s" (show c) (show rs)
  show (Z ls     (c:rs)) = printf "%s (%s) %s" (show (reverse ls)) (show c) (show rs)


toZipper :: [Char] -> Zipper Char
toZipper cs = Z [] cs

leftZ :: Zipper Char -> Zipper Char
leftZ (Z []     rs) = Z [] rs
leftZ (Z (l:ls) rs) = Z ls (l:rs)

rightZ :: Zipper Char -> Zipper Char
rightZ (Z ls []    ) = Z ls     []
rightZ (Z ls (r:rs)) = Z (r:ls) rs

focusZ :: Zipper Char -> Maybe Char
focusZ (Z []     []   ) = Nothing
focusZ (Z (l:_)  []   ) = Just l
focusZ (Z _      (r:_)) = Just r

isEmptyZ :: Zipper Char -> Bool
isEmptyZ (Z [] []) = True
isEmptyZ _         = False

isEndZ :: Zipper Char -> Bool
isEndZ (Z _ []) = True
isEndZ _        = False

removeZ :: Zipper Char -> Zipper Char
removeZ (Z []     []    ) = Z [] []
removeZ (Z []     [_]   ) = Z [] []
removeZ (Z (_:ls) []    ) = Z ls []
removeZ (Z ls     (_:rs)) = Z ls rs

lengthZ :: Zipper Char -> Int
lengthZ (Z ls rs) = length ls + length rs


testZipper :: IO ()
testZipper = do
  let inp = toZipper "abcdefghi"
      rs  = take 10 $ iterate rightZ inp
      ls  = take 10 $ iterate leftZ $ last rs
  mapM_ print rs
  mapM_ print ls


react :: Zipper Char -> Zipper Char
react zp@(Z _ []) = zp
react zp          = -- trace (show zp) $
  let mcc          = focusZ zp
      mcr          = focusZ . rightZ $ zp
      mIsReactable = isSameReactable <$> mcc <*> mcr
  in  case fromMaybe False mIsReactable of
        True  -> react . leftZ . removeZ . removeZ $ zp
        False -> react . rightZ $ zp


isSameReactable :: Char -> Char -> Bool
isSameReactable a b =
  toUpper a == b && toLower b == a ||
  toUpper b == a && toLower a == b


part1 :: IO ()
part1 = interact' "input05.txt" $ \line -> do
  let zp = toZipper line
      rz = react zp
  printf "Part1: Total reaction polymer %d\n" (lengthZ rz)



allCandidates :: String -> [String]
allCandidates s =
  let possibleUnits = nubOrd $ fmap toLower s
  in  fmap (\c -> filter ((/= c) . toLower) s) possibleUnits


part2 :: IO ()
part2 = interact' "input05.txt" $ \line -> do
  let zps  = fmap toZipper $ allCandidates line
      best = minimum $ fmap (lengthZ . react) zps
  printf "Part2: Best polymer result %d\n" best


testD :: IO ()
testD = do
  let inp = "bAcCaCBAcCcaA"
      zp  = react $ toZipper inp
  putStrLn inp
  putStrLn (show zp)


test :: IO ()
test = do
  let inps =
        [ "dabAcCaCBAcCcaDA"
        , "dabAaCBAcCcaDA"
        , "dabCBAcCcaDA"
        , "dabCBAcaDA" ]

  let zp1 = toZipper (inps !! 0)
      rz1 = react zp1
  printf "%s => %s\n" (show zp1) (show rz1)
  printf "%d\n" (lengthZ rz1)
  let cdds = allCandidates (inps !! 0)
  mapM_ print cdds

  let zps = fmap toZipper $ allCandidates (inps !! 0)
      rzs = fmap (\zp -> (zp, react zp, lengthZ $ react zp)) zps
      best = minimumOn (\(_, _, len) -> len)  rzs
  mapM_ print rzs
  print "-----best-------"
  print best
  pure ()
