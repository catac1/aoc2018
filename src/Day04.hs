module Day04
  ( test
  ) where


import Lib
import Data.Map qualified as M
import Data.Map (Map)


data Log = L
  { timestamp :: UTCTime
  , guardId   :: Int
  , action    :: Action
  } deriving stock (Eq)


instance Show Log where
  show (L stamp gid action) =
    let tstr = formatTime defaultTimeLocale "%Y-%-m-%-d %H:%M" stamp
    in  printf "L| %s #%d %s" tstr gid (show action)


data Action = Shift | Asleep | Wakeup
  deriving stock (Show, Eq)


newtype DutyPattern = DP [Int]


instance Show DutyPattern where
  show (DP xs) = printf "DP: %s" (concatMap show xs)

instance Semigroup DutyPattern where
  (DP as) <> (DP bs) = DP $ zipWith (+) as bs

instance Monoid DutyPattern where
  mempty = DP $ (take 60 $ repeat 0)


parseTimestamp :: String -> (UTCTime, String)
parseTimestamp s =
  let [tstr, rest] = splitOn "] " s
      timestamp    = fromMaybe (error "time parse error")
                     $ parseTimeM True defaultTimeLocale "%Y-%-m-%-d %H:%M"
                     $ tail tstr
  in  (timestamp, rest)


parseGuardIdNAction :: String -> (Maybe Int, Action)
parseGuardIdNAction s =
  let actionStr = reverse . takeWhile (/= ' ') . reverse $ s
      act       = parseAction actionStr
  in  case act of
        Shift ->
          let guardIdStr = takeWhile isDigit $ tail $ dropWhile (/= '#') $ s
          in  (Just $ read guardIdStr, act)
        _     -> (Nothing, act)


testParse :: IO ()
testParse = do
  let inps =
        [ "[1518-11-01 00:00] Guard #10 begins shift"
        , "[1518-11-01 00:05] falls asleep"
        , "[1518-11-01 00:25] wakes up" ]
  let ts  = fmap parseTimestamp inps
      gas = fmap (parseGuardIdNAction . snd) ts
  mapM_ print ts
  mapM_ print gas
  print $ length ts
  pure ()


parseAction :: String -> Action
parseAction s = case s of
  "shift"  -> Shift
  "asleep" -> Asleep
  "up"     -> Wakeup
  _        -> error "Action parse error"


parseLogs :: [String] -> [Log]
parseLogs ss =
  let sortedLogs    = sortOn fst $ fmap parseTimestamp ss
      (t0, s0)      = head sortedLogs
      (mGid0, act0) = parseGuardIdNAction s0
      gid0          = fromMaybe (error "first log should be Shift") mGid0
  in  (L t0 gid0 act0) : go gid0 sortedLogs
  where
    go :: Int -> [(UTCTime, String)] -> [Log]
    go _   []          = []
    go gid ((t,s):tss) =
      let (mGid, act) = parseGuardIdNAction s
          gid'        = fromMaybe gid mGid
      in (L t gid' act) : go gid' tss


totalWakeupTime :: [Log] -> Map Int DiffTime
totalWakeupTime logs = go 0 0 logs M.empty
  where
    go :: Int -> DiffTime -> [Log] -> Map Int DiffTime -> Map Int DiffTime
    go _   _      []      m = m
    go gid casleep (l:ls) m =
      case action l of
        Shift  -> go (guardId l) 0 ls m
        Asleep -> let timestamp' = (utctDayTime $ timestamp l)
                  in  go gid timestamp' ls  m
        Wakeup -> let m' = M.alter (alterf casleep (utctDayTime $ timestamp l)) gid m
                  in  go gid 0 ls m'

    alterf :: DiffTime -> DiffTime -> Maybe DiffTime -> Maybe DiffTime
    alterf asleep wakeup alseepTotal =
      let duration = wakeup - asleep
      in  Just $ duration + fromMaybe 0 alseepTotal


dutyPattern :: Int -> [Log] -> DutyPattern
dutyPattern gid logs =
  let guardLogs = filter (not . null)
                  $ splitWhen (\l -> action l == Shift)
                  $ filter ((== gid) . guardId) logs
  in  foldMap mergeShiftLog guardLogs


groupLogs :: Int -> [Log] -> [[Log]]
groupLogs gid logs =
  filter (not . null)
  $ splitWhen (\l -> action l == Shift)
  $ filter ((== gid) . guardId) logs


mergeShiftLog :: [Log] -> DutyPattern
mergeShiftLog lgs =
  let ranges     = fmap (\[a,b] -> (a, b))
                   $ chunksOf 2
                   $ fmap (minutes . timestamp) lgs
      dayRecord' = assert (all (\(a,b) -> a < b) ranges) $
                   fmap (updateDayRecord ranges) [0..59]
  in  DP dayRecord'
  where
    isInRange :: Int -> [(Int, Int)] -> Bool
    isInRange i ranges = any (\(asleep, wake) -> asleep <= i && wake > i) ranges

    updateDayRecord ranges i = if i `isInRange` ranges then 1 else 0


minutes :: UTCTime -> Int
minutes t = read $ formatTime defaultTimeLocale "%M" t


part1 :: IO ()
part1 = interactS "input04.txt" $ \ls -> do
  let logs           = sortOn timestamp $ parseLogs ls
      m              = totalWakeupTime logs
      longestSleeper = fst $ maximumOn snd (M.toList m)
      (DP pattern)   = dutyPattern longestSleeper logs
      bestSneakIn    = fst $ last $ sortOn snd $ zip [0..] pattern
      ans            = longestSleeper * bestSneakIn
  printf "Guard ID: %d, Best Time: %d => %d\n" longestSleeper bestSneakIn ans
  pure ()


maxSleepMinute :: DutyPattern -> (Int, Int)
maxSleepMinute (DP pttrn) = last $ sortOn snd $ zip [0..] pttrn


part2 :: IO ()
part2 = interactS "input04.txt" $ \ls -> do
  let logs        = sortOn timestamp $ parseLogs ls
      gids        = fmap guardId logs
      (g, (m, f)) = last
                    $ sortOn (\(_gid, (_minute, frequency)) -> frequency)
                    $ fmap (\gid -> (gid, maxSleepMinute $ dutyPattern gid logs)) gids
  printf "Part2: Guard ID: %d, Choice of minute: %d, Sleep Frequency %d\n" g m f
  printf "Part2: %d\n" (g * m)


test :: IO ()
test = do
  let inps =
        [ "[1518-11-01 00:00] Guard #10 begins shift"
        , "[1518-11-01 00:05] falls asleep"
        , "[1518-11-01 00:25] wakes up"
        , "[1518-11-01 00:30] falls asleep"
        , "[1518-11-01 00:55] wakes up"
        , "[1518-11-01 23:58] Guard #99 begins shift"
        , "[1518-11-02 00:40] falls asleep"
        , "[1518-11-02 00:50] wakes up"
        , "[1518-11-03 00:05] Guard #10 begins shift"
        , "[1518-11-03 00:24] falls asleep"
        , "[1518-11-03 00:29] wakes up"
        , "[1518-11-04 00:02] Guard #99 begins shift"
        , "[1518-11-04 00:36] falls asleep"
        , "[1518-11-04 00:46] wakes up"
        , "[1518-11-05 00:03] Guard #99 begins shift"
        , "[1518-11-05 00:45] falls asleep"
        , "[1518-11-05 00:55] wakes up" ]

  let logs = sortOn timestamp $ parseLogs inps
  let m = totalWakeupTime logs
  let longestSleeper = fst . last $ sortOn snd (M.toList m)
  printf "longestSleeper: #%d\n" longestSleeper
  let dp = dutyPattern longestSleeper logs
  print dp
  let dp2  = dutyPattern 99 logs
  print dp2

  let foo =groupLogs 99 logs
  forM_ foo $ \ lg -> do
    mapM_ print lg

  let gids        = fmap guardId logs
      (g, (m, f)) = last
                    $ sortOn (\(_gid, (_minute, frequency)) -> frequency)
                    $ fmap (\gid -> (gid, maxSleepMinute $ dutyPattern gid logs)) gids
  printf "Part2: Guard ID: %d, Choice of minute: %d, Sleep Frequency %d\n" g m f
  printf "Part2: %d\n" (g * m)
  pure ()
