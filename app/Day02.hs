module Main where

import Control.Applicative
import Control.Lens
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Map (Map, insertWith, toList)
import Data.Text (Text)
import qualified Data.Text as Text

import Debug.Trace

import Lib (someFunc)

main :: IO ()
main =
  interact mySolution

findAllCounts :: String -> Map Char Int
findAllCounts = foldr (\a -> insertWith (+) a 1) mempty

findNum :: Int -> Map Char Int -> Int
findNum num ma =
  case filter (== num) . fmap snd $ toList ma of
    [] -> 0
    _ -> 1

getCounts :: String -> (Int, Int)
getCounts s =
  let aCounts = findAllCounts s
  in (findNum 2 aCounts, findNum 3 aCounts)

totalCount :: [(Int, Int)] -> Int
totalCount is =
  let (totalTwo, totalThree) = foldr (\(two, three) (accTwo, accThree) -> (accTwo + two, accThree + three)) (0, 0) is
  in totalTwo * totalThree

mySolution :: String -> String
mySolution inputStr =
  let l = lines inputStr
  in show $ totalCount $ fmap getCounts l
