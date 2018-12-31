module Main where

-- common imports that are often used
import Control.Lens
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text as Text

-- you can import from modules defined in src/
import Lib (someFunc)


main :: IO ()
main =
  -- I'd recommend using the interact function for most of the problems.
  -- The type of interact is (String -> String) -> IO ().
  --
  -- interact takes a function that takes an input String and returns an
  -- output String.  The input String is the String read from stdin.  The
  -- output String will be printed to stdout.
  --
  -- It is an easy way to do input and output without having to manually
  -- read and write from stdin/stdout.  interact is nice for using to do
  -- these types of programming competitions.
  --
  -- You may also be interested in Data.ByteString.interact or
  -- Data.Text.IO.interact, which work on ByteString and Text respectively.
  interact mySolution

-- | This is a function that has been passed to interact above.
--
-- This is an example of just reading in a String from stdin and reversing it.
-- The reversed String will be output to stdout.
mySolution :: String -> String
mySolution inputStr = reverse inputStr

-- | This is an example of using doctest.  This is easy to use to test
-- functions you are working on.  It is much lighter weight than using
-- some sort of unit testing framework.
--
-- Basically you just write three greater than signs and an example of calling
-- the function you want to test.  Then on the next line you write what the
-- function returns.
--
-- >>> doctestExample 3
-- 13
--
-- You can run these doctests by running `stack test --fast`.
--
-- You can find out more information about doctest at the following page:
--
-- https://github.com/sol/doctest
doctestExample :: Int -> Int
doctestExample i = i + 10
