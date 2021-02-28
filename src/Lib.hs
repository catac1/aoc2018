module Lib
  ( module Control.Monad
  , module Control.Monad.ST
  , module Control.Exception
  , module Control.Applicative
  , module Data.List
  , module Data.List.Split
  , module Data.List.Extra
  , module Data.Char
  , module Data.Tuple
  , module Data.Ratio
  , module Data.Foldable
  , module Data.Maybe
  , module Text.Printf
  , module Debug.Trace
  , module Data.Time
  , module Data.Discrimination
  , readLines
  , readBlocks
  , interactF
  , interact'
  , interactS
  , interactSS
  , extractJust
  , branch
  , minimumsOn
  , maximumsOn
  , safeTail
  , tfst
  , tsnd
  , ttrd
  ) where


import Control.Monad
import Control.Monad.ST
import Control.Exception
import Control.Applicative
import Data.List hiding (nub, group)
import Data.List.Split (splitWhen)
import Data.List.Extra hiding (nub, sort)
import Data.Discrimination hiding (sort, group)
import Data.Char
import Data.Tuple
import Data.Ratio
import Data.Maybe
import Data.Foldable
import System.IO
import Control.Exception
import System.FilePath
import Text.Printf
import Debug.Trace
import Data.Time


readLines :: Handle -> IO [String]
readLines hndl =
  hIsEOF hndl >>= \case
    True -> pure []
    False -> (:) <$> hGetLine hndl <*> readLines hndl


readBlocks :: Handle -> IO [[String]]
readBlocks hndl = fmap (splitOn [""]) (readLines hndl)


interactF :: FilePath -> (Handle -> IO a) -> IO a
interactF filename body =
  bracket (openFile ("inputs" </> filename) ReadMode) hClose body


interact' :: FilePath -> (String -> IO a) -> IO a
interact' filename body =
  bracket (openFile ("inputs" </> filename) ReadMode)
          hClose  $ \hndl -> readLines hndl >>= body . intercalate "\n"


interactS :: FilePath -> ([String] -> IO a) -> IO a
interactS filename body =
  bracket (openFile ("inputs" </> filename) ReadMode)
          hClose  $ \hndl -> readLines hndl >>= body


interactSS :: FilePath -> ([[String]]-> IO a) -> IO a
interactSS filename body =
  bracket (openFile ("inputs" </> filename) ReadMode)
          hClose $ \hndl -> readBlocks hndl >>= body


extractJust ::  [Maybe a] -> [a]
extractJust = fmap fromJust . filter isJust


branch :: a -> a -> Bool -> a
branch e1 _  True  = e1
branch _  e2 False = e2


minimumsOn :: Ord b => (a -> b) -> [a] -> [a]
minimumsOn f xs0 = mumsOn minimumOn f xs0


maximumsOn :: Ord b => (a -> b) -> [a] -> [a]
maximumsOn f xs0 = mumsOn maximumOn f xs0


mumsOn
  :: Ord b
  => ((a -> b) -> [a] -> a)
  -> (a -> b)
  -> [a] -> [a]
mumsOn mumOn f xs0 =
  let mumV = mumOn f xs0
  in  go (f mumV) xs0
  where
    go _    []     = []
    go mumV (x:xs) =
      if f x == mumV then x : go mumV xs else go mumV xs

safeTail :: [a] -> [a]
safeTail []     = []
safeTail (x:xs) = xs

tfst :: (a, b, c) -> a
tfst (a, b, c) = a

tsnd :: (a, b, c) -> b
tsnd (a, b, c) = b

ttrd :: (a, b, c) -> c
ttrd (a, b, c) = c
