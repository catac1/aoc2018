module Day13
  ( test
  ) where


{-

 It's a simulation problem.


 1. Model tracks.
 2. Model cart movement.
 3. Check the collisions of the carts on each tick.

  Do we need to use Vector for the modeling of tracks?
   - I think the answer is YES.


-}


import Lib
import Data.Vector qualified as V
import Data.Vector (Vector)


data Track = T
  { trR :: Vector Char
  , trW :: Int
  , trH :: Int
  }


data Pos = P
  { px :: Int
  , py :: Int
  } deriving stock (Eq)


instance Show Pos where
  show (P x y) = printf "P| %2d %2d" x y


instance Ord Pos where
  compare (P x1 y1) (P x2 y2) =
    let cmpy = compare y1 y2
    in  if cmpy == EQ then compare x1 x2 else cmpy


data Dir = N | E | W | S
  deriving stock (Eq,Show)

data Turn = ToL | ToR | ToS
  deriving stock Show


data Cart = C
  { cPos  :: Pos
  , cDir  :: Dir
  , cTurn :: Turn
  } deriving stock Show


instance Eq Cart where
  (C pos1 _ _) == (C pos2 _ _) = pos1 == pos2

instance Ord Cart where
  compare (C pos1 _ _) (C pos2 _ _) = compare pos1 pos2


nextCartState :: Track -> Cart -> Cart
nextCartState (T vTrack w h) (C pos dir turn) =
  let currTrackChar = vTrack V.! (toIdx w pos)
  in  case currTrackChar of
        '-'  -> C (movePos dir pos) dir turn
        '|'  -> C (movePos dir pos) dir turn
        '/'  -> case dir of
                  N -> C (movePos E pos) E turn
                  W -> C (movePos S pos) S turn
                  S -> C (movePos W pos) W turn
                  E -> C (movePos N pos) N turn
        '\\' -> case dir of
                  N -> C (movePos W pos) W turn
                  E -> C (movePos S pos) S turn
                  W -> C (movePos N pos) N turn
                  S -> C (movePos E pos) E turn
        '+'  -> case turn of
                  ToS -> C (movePos dir pos) dir ToR
                  ToL -> case dir of
                           N -> C (movePos W pos) W ToS
                           E -> C (movePos N pos) N ToS
                           S -> C (movePos E pos) E ToS
                           W -> C (movePos S pos) S ToS
                  ToR -> case dir of
                           N -> C (movePos E pos) E ToL
                           E -> C (movePos S pos) S ToL
                           S -> C (movePos W pos) W ToL
                           W -> C (movePos N pos) N ToL
        _     -> error "unknown track character"


movePos :: Dir -> Pos -> Pos
movePos dir (P x y) = case dir of
  N -> P x (y - 1)
  S -> P x (y + 1)
  E -> P (x + 1) y
  W -> P (x - 1) y


renderCart :: Cart -> Char
renderCart (C _ dir _) = case dir of
  N -> '^'
  E -> '>'
  W -> '<'
  S -> 'v'


toIdx :: Int -> Pos -> Int
toIdx width (P x y) = y * width + x


fromIdx :: Int -> Int -> Pos
fromIdx width i = let (y, x) = i `divMod` width in P x y


parse :: [String] -> (Track, [Cart])
parse ss =
  let width      = maximum $ fmap length ss
      height     = length ss
      pss        = [P x y | y <- [0..height-1], x <- [0..width-1]]
      emptyTrack = V.replicate (width * height) ' '
      trackData  = zipWith (\p c -> (toIdx width p, toTrackChar c)) pss (concat ss)
      carts      = fmap toCart $ filter (\(_, c) -> c `elem` ['v','<','>', '^']) (zip pss (concat ss))
      track      = T (emptyTrack V.// trackData) width height
  in  (track, carts)
  where
    toCart (pos, c) = case c of
      '^' -> C pos N ToL
      'v' -> C pos S ToL
      '<' -> C pos W ToL
      '>' -> C pos E ToL
      _   -> error "invalid cart state"
    toTrackChar c = case c of
      '^' -> '|'
      'v' -> '|'
      '<' -> '-'
      '>' -> '-'
      _   -> c


renderTrack :: Track -> String
renderTrack track =
  let cs = V.toList (trR track)
  in  foldMap (<> "\n") (chunksOf (trW track) cs)


renderSim :: Track -> [Cart] -> String
renderSim track carts =
  let width   = trW track
      cartPss = fmap (\cart -> (toIdx width $ cPos cart, renderCart cart))  carts
      sim     = V.toList $ (trR track) V.// cartPss
  in  foldMap (<> "\n") (chunksOf width sim)

-- BUG: We simulates the cart locations at the same time.
--      this can't check collison of the carts which are located side by side to each other.
--
--      eg) ---><---  =>  ---<>---
--
-- We must check collision of the carts on every movement.
-- Cart movement order is defined from the problem condition.
-- top left to bottom right.

simulate :: Track -> (Maybe Pos ,[Cart]) -> (Maybe Pos, [Cart])
simulate track (_, carts0) =
  let sortedCarts     = sort carts0
      partialSimCarts = fmap (partialSim sortedCarts) [1..length carts0]
      mCollisonPos    = asum $ fmap detectCartCrash partialSimCarts
  in  (mCollisonPos, last partialSimCarts)
  where
    partialSim carts i =
      let (left, right) = splitAt i carts
      in  (fmap (nextCartState track) left) <> right

{-
   Welp, I give up. I can't find the bug. :<
   Anyway Cheers See you next time.

---------------------------------------------------------

  The Bug:
    The cart shouldn't be added when collision exists on either carts.
    but, I erroneously added the the cart onto the nextCarts when the collision
    occured in the currCarts.

    currCarts : The carts will be simulated in a tick.
    nextCarts : The carts already simulated in a tick.
-}

simulateWithRemoval :: Track -> [Cart] -> [Cart]
simulateWithRemoval track carts0 =
  let sortedCarts = sort carts0
  in  go sortedCarts []
  where
    go []               nextCarts = nextCarts
    go (cart:currCarts) nextCarts =
      -- Is there any case where cart' exists in both carts and simulatedCarts?
      -- No, because all of the cart0 locations are unique and we remove
      -- any collided carts in previous recursion on both lists.
      let cart'           = nextCartState track cart
          collidedCurr    = cart' `elem` currCarts
          collidedNext    = (not collidedCurr) && (cart' `elem` nextCarts)

          currCarts'      = if | collidedCurr -> delete cart' currCarts
                               | otherwise    -> currCarts

          nextCarts'      = if | collidedNext -> delete cart' nextCarts
                               | collidedCurr -> nextCarts
                               | otherwise    -> cart':nextCarts
      in  go currCarts' nextCarts'


lastStandingCart :: Track -> [Cart] -> Cart
lastStandingCart track carts =
  head . head $ dropWhile ((/= 1) . length) $ iterate (simulateWithRemoval track) carts


collisionRecords :: Track -> [Cart] -> [[Cart]]
collisionRecords track carts =
  iterate (simulateWithRemoval track) carts


detectCartCrash :: [Cart] -> Maybe Pos
detectCartCrash carts =
  fmap head $ find ((> 1) . length) $ group $ sort $ fmap cPos carts


firstCrash :: Track -> [Cart] -> Pos
firstCrash track carts0 =
  let simulations = iterate (simulate track) (Nothing, carts0) :: [(Maybe Pos, [Cart])]
      mFirstCrash = fst . head $ dropWhile (isNothing . fst) simulations
  in  fromMaybe (P 0 0) mFirstCrash


part1 :: IO ()
part1 = interactS "input13.txt" $ \lines -> do
  let (track, carts0) = parse lines
      cartsN          = iterate (simulate track) (Nothing, carts0)
      pos             = firstCrash track carts0
  printf "Part1: The first cart crash occurs at %d,%d\n" (px pos) (py pos)


part2 :: IO ()
part2 = interactS "input13.txt" $ \lines -> do
  let (track, carts0) = parse lines
      (C pos _ _)     = lastStandingCart track carts0
  printf "Part2: The last cart is located at %d,%d\n" (px pos) (py pos)


testCol :: IO ()
testCol = do
  let inp1 =
        [ "|||||"
        , "|v|||"
        , "+++++"
        , "|||^|"
        , "|||||"]
      inp2 =
        [ "/-\\  "
        , "\\>+-\\"
        , "  \\</"]
  let (track1, carts1) = parse inp2
  let lastCart = lastStandingCart track1 carts1
  let records  = collisionRecords track1 carts1
  -- let len = length $ takeWhile ((> 1) . length) $ records
  let len = 5
  mapM_ (\carts -> do
            print "----"
            mapM_ print carts
            print "----"
        ) $ take (len + 1) records

  let cartsN = take 15 $iterate (simulate track1) (Nothing, carts1)

  let cartsStates = fmap snd cartsN
  mapM_ putStrLn $ fmap (renderSim track1) cartsStates




test :: IO ()
test = do
  let inp1 =
        [ "/->-\\        "
        , "|   |  /----\\"
        , "| /-+--+-\\  |"
        , "| | |  | v  |"
        , "\\-+-/  \\-+--/"
        , "  \\------/   "]
      inp2 =
        [ "/>-<\\  "
        , "|   |  "
        , "| /<+-\\"
        , "| | | v"
        , "\\>+</ |"
        , "  |   ^"
        , "  \\<->/"]

  let (track, carts1) = parse inp1
  putStrLn $ renderTrack track
  mapM_ print carts1
  let cartsN = take 15 $iterate (simulate track) (Nothing, carts1)

  -- forM_ (zip ([0..] :: [Int]) cartsN) $ \(i, carts) -> do
  --   printf " --- Tick %2d ---\n" i
  --   putStrLn $ renderSim track carts
  --   print $ detectCartCrash carts

  print $ firstCrash track carts1

  let (track2, carts2) = parse inp2
  let lastCart = lastStandingCart track2 carts2
  putStrLn " --- Last Cart --- "
  print lastCart
  let records = collisionRecords track2 carts2
  let len = length $ takeWhile ((> 1) . length) $ records
  mapM_ (\carts -> do
            print "----"
            mapM_ print carts
            print "----"
        ) $ take (len + 1) records
  pure ()
