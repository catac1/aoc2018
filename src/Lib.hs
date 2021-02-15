module Lib
  ( module Control.Monad
  , module Control.Monad.ST
  , module Control.Exception
  , module Control.Applicative
  , module Data.List
  , module Data.List.Split
  , module Data.Char
  , module Data.Foldable
  , module Data.Maybe
  , module Text.Printf
  , module Debug.Trace
  , readLines
  , readBlocks
  , interactF
  , interact'
  , interactS
  , interactSS
  , extractJust
  , branch
  ) where


import Control.Monad
import Control.Monad.ST
import Control.Exception
import Control.Applicative
import Data.List
import Data.List.Split
import Data.Char
import Data.Maybe
import Data.Foldable
import System.IO
import Control.Exception
import System.FilePath
import Text.Printf
import Debug.Trace


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

